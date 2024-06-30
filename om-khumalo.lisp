(in-package om)

(defmethod! norm-rhythm ((cseq1 chord-seq) (cseq2 chord-seq))
  :icon 209
  :indoc '("a chord-seq" "a chord-seq")
  :doc
"alters the chord-seq with fewer notes to have the same number of
notes as the one with more notes.
the altered chord-seq will have identical notes piled on top of each
other, each representing a note
in the other chord-seq that is closest to it (measured by proportional
position within the sequence,
not absolute ms.)   meant to be fed into interpolate-rhythms"

  :numouts 2
  (if (= (length (lonset cseq1)) (length (lonset cseq2)))
      (values cseq1 cseq2)

    (let* (short-onsets
           short-durs
           long-onsets
           long-durs
           shorter-cseq)

      (if (< (length (lonset cseq1)) (length (lonset cseq2)))
          (setf short-onsets (lonset cseq1) short-durs (flat (ldur cseq1))
                long-onsets (lonset cseq2) long-durs (flat (ldur cseq2))
                shorter-cseq cseq1)

        (setf short-onsets  (lonset cseq2) short-durs (flat (ldur cseq2))
              long-onsets (lonset cseq1) long-durs (flat (ldur cseq1))
              shorter-cseq cseq2))

      (let* ((short-len (+ (last-elem short-onsets) (last-elem short-durs)))
             (long-len (+ (last-elem long-onsets) (last-elem long-durs)))
             (stretched-onsets (om-scale short-onsets 0 long-len 0 short-len)))

    ;list of indeces, one for each note of the long rhythm, telling
    ;which note of the short rhythm it corresponds to.
        (let* ((short-correspondences (loop with temp = (copy-list long-onsets)
                                            for s-onset in stretched-onsets
                                            collect 
                                            (progn
                                              (setf temp (sort temp '< :key #'(lambda (l-onset) (abs (- l-onset s-onset)))))
                                              (pop temp))))

               (closest-indeces ;;; should be called long-correspondences
                                (loop for l-onset in long-onsets
                                      collect (or (position l-onset short-correspondences)
                                                  (first (sort (copy-list stretched-onsets) '< :key #'(lambda (s-onset) (abs (- s-onset l-onset)))))))))

          (loop for midic in (lmidic shorter-cseq)
                for s-onset in short-onsets
                for s-dur in short-durs
                for index from 0
                for matches = (count-if #'(lambda (i) (= i index)) closest-indeces)

                append (create-list matches s-onset) into new-onsets
                append `(,@(create-list (1- matches) 0) ,s-dur) into new-durs
                append (create-list matches midic) into new-pitches

                finally return  (let ((new-cseq
                                       (mki 'chord-seq
                                            :lmidic new-pitches
                                            :lonset new-onsets
                                            :ldur new-durs)))
                                  (if (< (length (lonset cseq1)) (length (lonset cseq2)))
                                      (values new-cseq cseq2)
                                    (values cseq1 new-cseq)))))))))


(defmethod! dur->nsamples ((cseq1 chord-seq) (cseq2 chord-seq) (time number))
  :icon 209
  :indoc '("a chord-seq" "a chord-seq" "time in ms")
  :doc "A utility to be used alongside interpolate-rhythms. It gives
the number of samples to be given to interpolate-rhythms to
result in a sequence of a given length."
  (om-round (float (/ (* 2 time)
                       (+ (+ (first (last-n (lonset cseq1) 2))
                                    (car (last-elem (ldur cseq1))))
                          (+ (first (last-n (lonset cseq2) 2))
                                    (car (last-elem (ldur cseq2)))))))
            2))





(defmethod! interpolate-rhythms ((cseq1 chord-seq) (cseq2 chord-seq)
max-duration &optional (curve 0.0) (samples-1 5) (concat? t)
                            )
  :initvals (list nil nil nil 1.0 5 t)
  :icon 209
  :indoc '("a chord-seq" "a chord-seq" "time in ms" "interpolation
curve" "fixed number of samples" "concatenate into single chord-seq?")
  :doc
"provided that both chord-seqs have the same number of notes
(as prepared by norm-rhythms) this will interpolate rhythms and
pitches of the two sequences. max-duration will result in the
number of interpolation samples being determined by dur->nsamples.

optional arguments:
curve (same as interpolation function)

samples-1 : explicit number of samples, instead of max-duration
     (but max-duration takes precedence)

concat? :  if this is t, then the resulting interpolation steps will
     be concatenated into one chord-seq. if it is nil,
     then a list of chord-seqs is returned"

(let* ((samples (if max-duration
                    (floor (dur->nsamples cseq1 cseq2 max-duration))
                  samples-1))
       (result (loop for midic-list in (interpolation (lmidic cseq1)
                                                      (lmidic cseq2) samples curve)
                     for onset-list in (interpolation (lonset cseq1)
                                                      (lonset cseq2) samples curve)
                     for dur-list in (interpolation (ldur cseq1)
                                                    (ldur cseq2) samples curve)
                     collect (mki 'chord-seq
                                  :lmidic (om-round midic-list)
                                  :lonset (om-round onset-list)
                                  :ldur (om-round dur-list)
                                  ))))
  (if concat?
      (reduce #'concat result)
    result)))
