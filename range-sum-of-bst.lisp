(require :dc-ds)

(defun range-sum-bst (root l r)
  (loop with queue = (list root)
     for node = (pop queue)
     while node
     for visited = 0 then (1+ visited)
     for right = (right node)
     for left = (left node)
     for value = (value node)
     ;; when (and left (> value l))  do (push left queue)
     ;; when (and right (< value r)) do (push right queue)
     when left  do (push left queue)
     when right do (push right queue)
     when (and (<= value r) (>= value l)) summing value into sum
     finally (return (list :sum sum :visited visited))))
