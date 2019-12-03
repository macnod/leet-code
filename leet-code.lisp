(in-package :leet-code)

(defun palindrome-number (number)
  (let ((digits (loop 
                   for e from (truncate (log number 10)) downto 0
                   for n = number then (- n (* digit p))
                   for p = (expt 10 e)
                   for digit = (truncate n p)
                   collect digit)))
    (equal digits (reverse digits))))

(defun merge-sorted-arrays (nums1 m nums2 n)
  (loop with j = 0
     for i = 0 then (1+ i)
     for n1 = (elt nums1 i)
     for n2 = (when (< j n) (elt nums2 j))
     while (or (< i m) (< j n))
     when (and n2 (<= n2 n1))
     do (loop for k from m downto (1+ i)
           do (setf (elt nums1 k) (elt nums1 (1- k)))
           finally 
             (setf (elt nums1 i) n2)
             (incf j)
             (incf m))
     finally (loop for k from j below n
                for l = m then (1+ l)
                do (setf (elt nums1 l) (elt nums2 k)))
       (return nums1)))

(defun merge-sorted-lists (list1 list2)
  (cond ((null list1) list2)
        ((null list2) list1)
        ((<= (car list2) (car list1))
         (cons (car list2) (merge-sorted-lists list1 (cdr list2))))
        (t (cons (car list1) (merge-sorted-lists (cdr list1) list2)))))

(defun merge-sorted-arrays-1 (nums1 m nums2 n)
  (loop with len = (+ m n)
     and mm = (1- m)
     and nn = (1- n)
     while (not (zerop len))
     do (decf len)
       (if (or (< nn 0) (> (elt nums1 mm) (elt nums2 nn)))
           (progn 
             (setf (elt nums1 len) (elt nums1 mm))
             (decf mm))
           (progn
             (setf (elt nums1 len) (elt nums2 nn))
             (decf nn)))
     finally (return nums1)))
       
(defun reorder-log-files (list)
  (destructuring-bind (n-logs l-logs)
      (partition-by-lambdas list (lambda (x) (scan "^[^ ]+ [0-9]" x)) :other t)
    (append (sort l-logs #'string< :key 
                  (lambda (x) 
                    (format nil "~{~a~^ ~}" (reverse (split " " x :limit 2)))))
            n-logs)))

(defun move-zeros (array)
  (loop with j = 0
     for i from 0 below (length array)
     unless (zerop (aref array i))
     do (setf (aref array j) (aref array i))
       (setf (aref array i) (if (= j i) (aref array i) 0))
       (incf j)
     finally (return array)))
       
(defun most-common-word (paragraph banned)
  (loop with h-banned = (loop with h = (make-hash-table :test 'equal)
                           for word in banned do (setf (gethash word h) t)
                           finally (return h))
     with words = (mapcar #'string-downcase (split "[^-a-zA-Z0-9]+" paragraph))
     with count = (make-hash-table :test 'equal)
     for word in words
     unless (gethash word h-banned)
     do (incf (gethash word count 0))
     finally (return (car (sort words #'> 
                                :key (lambda (w) (gethash w count 0)))))))

(defun min-change (s1 s2 &optional (changes 0))
  (when (stringp s1)
    (setf s1 (ppcre:split "" s1)
          s2 (ppcre:split "" s2)))
  (when (< (length s2) (length s1))
    (rotatef s1 s2))
  (cond ((or (null s1) (null s2))
         (+ changes (length s1) (length s2)))
        ((equal (car s1) (car s2))
         (min-change (rest s1) (rest s2) changes))
        (t (if (> (length s2) (length s1))
               (push (car s2) s1)
               (setf (car s1) (car s2)))
           (min-change (rest s1) (rest s2) (1+ changes)))))

(defun min-change-tests ()
  (loop for (s1 s2 expected) in 
       '(("abc" "abc" 0)
         ("ac" "abc" 1)
         ("abc" "ac" 1)
         ("" "abc" 3)
         ("abc" "" 3)
         ("abc" "cde" 3)
         ("asdfasfaadsf" "cdelksadas" 9))
     for result = (min-change s1 s2)
       collect (list :s1 s1 :s2 s2 :expected expected 
                     :result result :pass (equal expected result))))

(defclass dll-node ()
  ((value :accessor value :initarg :value :initform nil)
   (prev :accessor prev :initarg :prev :initform nil)
   (next :accessor next :initarg :next :initform nil)))

(defclass dll-list ()
  ((head :accessor head :initarg :head :initform nil)
   (tail :accessor tail :initarg :tail :initform nil)
   (len :accessor len :initform 0)))

(defmethod dll-push-head ((dll dll-list) (value t))
  (let ((old-head (head dll))
        (new-head (make-instance 'dll-node :value value)))
    (if old-head
        (progn
          (setf (prev old-head) new-head
                (next new-head) old-head
                (head dll) new-head)
          (when (= (len dll) 1)
            (setf (tail dll) old-head))
          (incf (len dll)))
        (setf (head dll) new-head
              (tail dll) new-head
              (len dll) 1))))

(defmethod dll-push-tail ((dll dll-list) (value t))
  (let ((old-tail (tail dll))
        (new-tail (make-instance 'dll-node :value value)))
    (if old-tail
        (progn
          (setf (next old-tail) new-tail
                (prev new-tail) old-tail
                (tail dll) new-tail)
          (when (= (len dll) 1)
            (setf (head dll) old-tail))
          (incf (len dll)))
        (setf (tail dll) new-tail
              (head dll) new-tail
              (len dll) 1))))

(defmethod dll-pop-head ((dll dll-list))
  (cond ((zerop (len dll)) nil)
        ((= (len dll) 1) (let ((node (head dll)))
                           (setf (head dll) nil
                                 (tail dll) nil
                                 (len dll) 0)
                           (value node)))
        ((= (len dll) 2) (let ((node (head dll)))
                           (setf (head dll) (next node)
                                 (tail dll) (next node)
                                 (next (head dll)) nil
                                 (prev (head dll)) nil
                                 (next (tail dll)) nil
                                 (prev (tail dll)) nil
                                 (len dll) 1)
                           (value node)))
        (t (let ((node (head dll)))
             (setf (head dll) (next node)
                   (prev (head dll)) nil)
             (decf (len dll))
             (value node)))))

(defmethod dll-pop-tail ((dll dll-list))
  (cond ((zerop (len dll)) nil)
        ((= (len dll) 1) (let ((node (tail dll)))
                           (setf (head dll) nil
                                 (tail dll) nil
                                 (len dll) 0)
                           (value node)))
        ((= (len dll) 2) (let ((node (tail dll)))
                           (setf (head dll) (prev node)
                                 (tail dll) (prev node)
                                 (next (head dll)) nil
                                 (prev (head dll)) nil
                                 (next (tail dll)) nil
                                 (prev (tail dll)) nil
                                 (len dll) 1)
                           (value node)))
        (t (let ((node (tail dll)))
             (setf (tail dll) (prev node)
                   (next (tail dll)) nil)
             (decf (len dll))
             (value node)))))

(defmethod dll-peek-head ((dll dll-list))
  (when (not (zerop (len dll)))
    (value (head dll))))

(defmethod dll-peek-tail ((dll dll-list))
  (when (not (zerop (len dll)))
    (value (tail dll))))

(defmethod dll-to-list ((dll dll-list))
  (loop for node = (head dll) then (next node)
     while node collect (value node)))

(defun dll-from-list (list)
  (loop with dll = (make-instance 'dll-list)
     for value in list do (dll-push-tail dll value)
     finally (return dll)))

(defclass max-stack ()
  ((stack-values :accessor stack-values :initarg :stack-values 
                 :initform (make-instance 'dll-list))
   (stack-max :accessor stack-max :initarg :stack-max
              :initform (make-instance 'dll-list))))

(defmethod max-stack-push ((stack max-stack) (value t))
  (let* ((max-node (dll-peek-head (stack-max stack)))
         (max (when max-node (value max-node))))
    (dll-push-head (stack-values stack) value)
    (when (or (null max) (>= value max))
      (dll-push-head (stack-max stack) (head (stack-values stack))))
    stack))

(defmethod max-stack-pop ((stack max-stack))
  (let ((max (value (dll-peek-head (stack-max stack))))
        (rv (dll-pop-head (stack-values stack))))
    (when (= rv max)
      (dll-pop-head (stack-values stack)))
    rv))

(defmethod max-stack-top ((stack max-stack))
  (dll-peek-head (stack-values stack)))

(defmethod max-stack-peek-max ((stack max-stack))
  (value (dll-peek-head (stack-max stack))))

(defmethod max-stack-pop-max ((stack max-stack))
  (let* ((max-node (dll-pop-head (stack-max stack)))
         (max-value (value max-node)))
    (cond ((= (dll-peek-head (stack-values stack)) max-value)
           (dll-pop-head (stack-values stack)))
          ((= (dll-peek-tail (stack-values stack)) max-value)
           (dll-pop-tail (stack-values stack)))
          (t (setf (next (prev max-node)) (next max-node))))
    max-value))

(defmethod max-stack-render ((stack max-stack))
  (let ((values (loop for node = (head (stack-values stack))
                   then (next node)
                   while node collect (value node)))
        (max (loop for node = (head (stack-max stack))
                then (next node)
                while node collect (value (value node)))))
    (list :values values :max max)))

(defmethod powers-of-2 ()
  (let ((filename (home-based "powers-of-two.dat")))
    (loop with powers-of-2 = (if (file-exists-p filename)
                                 (slurp-n-thaw filename)
                                 (loop for a from 1 to 30 collect 
                                      (list :power a :count 10)))
       for choices = (loop for item in powers-of-2
                        appending (loop for x from 1 to (getf item :count)
                                     collect item))
       for length = (length choices)
       for choice = (elt choices (random length))
       for power = (getf choice :power)
       for question = (format t "2^~a? " power)
       for answer-string = (read-line)
       for valid-answer = (and answer-string
                               (not (scan "^\s*$" answer-string)))
       for numeric-answer = (and valid-answer
                                 (scan "^\s*[0-9]+\s*" answer-string))
       for answer = (when numeric-answer (parse-integer answer-string))
       for correct-answer = (expt 2 (getf choice :power))
       for index = (1- power)
       while answer-string
       if (not numeric-answer)
       do (cond ((scan "^list" answer-string)
                 (format t "~{~a~%~}~%" 
                         (loop for x in
                              (sort (copy-seq powers-of-2)
                                    '> :key (lambda (x)
                                              (getf x :count)))
                            for power = (getf x :power)
                            for count = (getf x :count)
                            for total-count = (length choices)
                            for chances = (* (/ (float count)
                                                total-count)
                                             100)
                            collect (format nil "2^~2,'0d ~2d ~,2f%"
                                            power count chances))))
                ((scan "^quit" answer-string)
                 (format t "Thank you for playing.~%")
                 (return))
                (t (format t "Commands: list, quit~%")))
       else 
       if (= answer correct-answer) do 
         (format t "Correct!~%")
         (when (> (getf (elt powers-of-2 index) :count) 1)
           (decf (getf (elt powers-of-2 index) :count)))
       else do
         (format t "Incorrect!  2^~a = ~:d~%" power correct-answer)
         (when (< (getf (elt powers-of-2 index) :count) 40)
           (incf (getf (elt powers-of-2 index) :count)))
       end
       do (freeze-n-spew powers-of-2 filename))))

