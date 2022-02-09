(defvar condens-db nil "List of lists of Unicode NFKD-string and
matching unicode glyph. Generated with `condens-db-gen'.")

(defun condens-db-gen ()
  "Find all unicode glyphs that can be used for shortening text."
  (append
   ;; Some perfectly good ligatures don't have an ascii sequence as
   ;; NFKD! Hence add them manually. (There are more phonetic
   ;; etc. ligature-glyphs not like that, but the loop below finds
   ;; other glyphs for those same ascii sequences.)
   '(("ae" "æ") ("oe" "œ") ("oo" "ꝏ") ("ts" "ʦ") ("ls" "ʪ") ("ue" "ᵫ")
     ("aa" "ꜳ") ("ao"  "ꜵ") ("av" "ꜹ") ("uo" "ꭣ") ("pts" "₧") ("ar" "🜇")
     ("vb" "🝬") ("qp" "ȹ") ("tc" "ʨ"))
   (cl-loop for ch from 0 upto #x10ffff
            for nfkd = (ucs-normalize-NFKD-string ch)
            when (string-match (rx bol (>= 2 (in "a-zA-Z.,!?0-9")) eol) nfkd)
            collect (list nfkd (string ch)))))

(defun condens-find-all-substrs (needle haystack)
  (cl-loop with haystack-ind = 0
           for match-ind = (cl-search needle haystack :start2 haystack-ind
                                      :test #'char-equal)
           while match-ind
           do (setf haystack-ind (+ match-ind (length needle)))
           collect match-ind))

(defun condens-overlapping (candidates)
  (cl-loop for c1 in candidates
           for prev-c1-ind = -1 then c1-ind
           for rem-cand from 1
           for c1-ind = (first c1)
           for c1-ascii = (second c1)
           for c1-len = (length c1-ascii)
           when (/= c1-ind prev-c1-ind)
           collect (cons c1 (cl-loop for c2 in (subseq candidates rem-cand)
                                     for c2-ind = (first c2)
                                     when (< c2-ind (+ c1-ind c1-len))
                                     collect c2))))

(defun condens-update-choices-by-chosen (chosen from-group rest-choices)
  (let ((removed (cl-remove chosen from-group :test #'equalp)))
    (cl-loop for c in rest-choices
             if (find chosen c :test #'equalp)
             do (setf removed (append removed (cl-remove chosen c :test #'equalp)))
             else when (cl-set-difference c removed :test #'equalp)
             collect it)))

(defun condens-pick-choice (shortening-so-far choices)
  (if (<= (length choices) 1)
      ;; Last one, return the choice and how much the best choice will
      ;; shorten. Can be nil if no choices left - in that case,
      ;; additional shortening is 0 and choice is nil.
      (if choices
          (cl-loop for (i c u) in (car choices)
                   for c-len = (length c)
                   with max-len = -1
                   with best = nil
                   when (> c-len max-len)
                   do (setf best (list (+ shortening-so-far c-len -1) (list (list i c u))))
                   finally (return best))
        (list shortening-so-far (list nil)))
    ;; Not last one - recurse and pick best from the subtrees.
    (let ((first-group (car choices)))
      (assert first-group)
      (cl-loop for (i c u) in first-group
               for this-shortening = (1- (length c))
               for (shrt chosen) = (condens-pick-choice
                                    (+ this-shortening shortening-so-far)
                                    (condens-update-choices-by-chosen (list i c u)
                                                                      first-group
                                                                      (cdr choices)))
               with max-score = -1
               with best = nil
               when (> shrt max-score)
               do (setf max-score shrt
                        best (list shrt (append (list (list i c u)) chosen)))
               finally (return best)))))

(defun condens-apply-choices (name choices)
  (let (result
        (rem 0))
    (cl-loop for (i c u) in (remove nil choices)
             do
             (push (subseq name rem i) result)
             (push u result)
             (setf rem (+ i (length c))))
    (push (subseq name rem) result)
    (apply #'concatenate 'string (reverse result))))

(defun condens-str (str)
  (unless condens-db
    (message "Generating unicode ligature database, please wait...")
    (setf condens-db (condens-db-gen)))
  (let* ((candidates               ; will contain (index nfkd unicode)
          (cl-sort
           (cl-loop for (c u) in condens-db
                    for indices = (condens-find-all-substrs c str)
                    when indices
                    append (mapcar (lambda (x) (list x c u)) indices))
           #'< :key #'car))
         (grouped-overlap (condens-overlapping candidates))
         ;; From grouped-overlap, pick a combination that yields the
         ;; shortest result. Brute force.
         (chosen (condens-pick-choice 0 grouped-overlap)))
    (if (> (car chosen) 0)
        (message "Shortened %s by %d chars!" (string-trim str) (car chosen))
      (message "Couldn't shorten %s" (string-trim str)))
    (condens-apply-choices str (cadr chosen))))

(defun condens-this ()
  (interactive)
  (let ((beg (point)))
    (forward-word)
    (let* ((str (buffer-substring-no-properties beg (point)))
           (new (condens-str str)))
      (replace-region-contents beg (point) (lambda () new)))
    ;; Rest of this hairy buffer-jumping stuff tries to make
    ;; `condens-this' act like `capitalize-word'
    (forward-whitespace 1)
    (unless (= (point) (point-max)) (forward-whitespace -1))))
