;; condens.el

;; Copyright © 2022 Janne Nykopp

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(defvar condens-db nil "List of lists of Unicode NFKD-string and
matching unicode glyph. Generated with `condens-db-gen'.")

(defun condens-db-gen ()
  "Find all unicode glyphs that can be used for shortening text."
  (append
   ;; Some perfectly good ligatures don't have an ascii sequence as
   ;; NFKD! Hence add them manually. (There are more phonetic
   ;; etc. ligature-glyphs without NFKD which look like ascii
   ;; sequences, but the cl-loop below finds other glyphs for those
   ;; same ascii sequences.)
   '(("ae" "æ") ("oe" "œ") ("oo" "ꝏ") ("ts" "ʦ") ("ls" "ʪ") ("ue" "ᵫ")
     ("aa" "ꜳ") ("ao"  "ꜵ") ("av" "ꜹ") ("uo" "ꭣ") ("pts" "₧") ("ar" "🜇")
     ("vb" "🝬") ("qp" "ȹ") ("tc" "ʨ") ("ab" "🆎") ("cl" "🆑") ("cool" "🆒")
     ("free" "🆓") ("id" "🆔") ("new" "🆕") ("ng" "🆖") ("ok" "🆗") ("sos" "🆘")
     ("up!" "🆙") ("vs" "🆚") ("ic" "🆋") ("sa" "🆍") ("3d" "🆛") ("2ndscr" "🆜")
     ("2k" "🆝") ("4k" "🆞") ("8k" "🆟") ("5.1" "🆠") ("7.1" "🆡") ("22.2" "🆢")
     ("60p" "🆣") ("120p" "🆤") ("hc" "🆦") ("hdr" "🆧") ("hi-res" "🆨")
     ("lossless" "🆩") ("shv" "🆪") ("uhd" "🆫") ("vod" "🆬") ("lt" "₶") ("ce" "₠")
     ("nul" "␀") ("soh" "␁") ("stx" "␂") ("etx" "␃") ("eot" "␄") ("enq" "␅")
     ("ack" "␆") ("bel" "␇") ("bs" "␈") ("ht" "␉") ("lf" "␊") ("vt" "␋")
     ("cr" "␍") ("so" "␎") ("si" "␏") ("dle" "␐") ("dc1" "␑") ("dc2" "␒")
     ("dc3" "␓") ("dc4" "␔") ("nak" "␕") ("syn" "␖") ("etb" "␗") ("can" "␘")
     ("em" "␙") ("sub" "␚") ("esc" "␛") ("fs" "␜") ("gs" "␝") ("us" "␟")
     ("sp" "␠") ("del" "␡") ("nl" "␤") ("d.s." "𝄉") ("d.c." "𝄊") ("bb" "𝄫")
     ("8va" "𝄶") ("8vb" "𝄷") ("15ma" "𝄸") ("15mb" "𝄹") ("tr" "𝆖") ("ped" "𝆮")
     ("oy" "ѹ") ("xx" "⯵") ("60" "㉍") ("70" "㉎") ("80" "㉏") ("v/m" "㏞")
     ("a/m" "㏟") ("bl" "Ы") ("uh" "ﬕ") ("obj" "￼") ("sss" "∭") ("ssss" "⨌")
     ("1/2" "½") ("0/3" "↉") ("1/3" "⅓") ("2/3" "⅔") ("1/4" "¼") ("3/4" "¾")
     ("1/5" "⅕") ("2/5" "⅖") ("3/5" "⅗") ("4/5" "⅘") ("1/6" "⅙") ("5/6" "⅚")
     ("1/7" "⅐") ("1/8" "⅛") ("3/8" "⅜") ("5/8" "⅝") ("7/8" "⅞") ("1/9" "⅑")
     ("1/10" "⅒"))
   ;; Go through all of unicode except private use areas
   ;; #xe000-#xf8ff, #xf0000-#xffffd, #x100000-#x10fffd as they aren't
   ;; portable, and unicode noncharacters adjacent to them, #xffffe +
   ;; #xfffff, #x10fffe + #x10ffff as that simplifies the ranges
   ;; (planes 15 and 16 can be eliminated completely and search
   ;; stopped at plane 14). Begin is inclusive, end is exclusive.
   (let ((ranges '((0 . #xe000) (#xf900 . #xf0000))))
     (cl-loop for (lo . hi) in ranges append
              (cl-loop for ch from lo below hi
                       for nfkd = (ucs-normalize-NFKD-string ch)
                       when (string-match (rx bol (>= 2 (in "a-zA-Z.,!?/_0-9-")) eol) nfkd)
                       collect (list nfkd (string ch)))))))

(defun condens-find-all-substrs (needle haystack)
  (cl-loop with haystack-ind = 0
           for match-ind = (cl-search needle haystack :start2 haystack-ind
                                      :test #'char-equal)
           while match-ind
           do (setf haystack-ind (+ match-ind (length needle)))
           collect match-ind))

(defun condens-overlapping (candidates)
  ;; Assumes CANDIDATES are sorted with `condens-compare-candidates'
  ;; so that first candidate is always longest.
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

(defun condens-compare-candidates (c1 c2)
  ;; For easing overlappage check, sort primarily by index and
  ;; secondarily by length (longest first)
  (let ((c1-ind (first c1))
        (c2-ind (first c2)))
    (if (= c1-ind c2-ind)
        (> (length (second c1)) (length (second c2)))
      (< c1-ind c2-ind))))

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
           #'condens-compare-candidates))
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
  (skip-chars-forward "[:space:]")
  (let ((beg (point)))
    (forward-whitespace 1)
    (let ((end (point)))
      (let* ((str (buffer-substring-no-properties beg end))
             (new (condens-str str)))
        (replace-region-contents beg end (lambda () new))))
    ;; In string ending at end-of-buffer corner case, point won't move
    ;; after replace. Handle that.
    (when (= beg (point)) (forward-whitespace 1))))
