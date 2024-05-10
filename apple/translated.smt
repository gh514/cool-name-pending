(set-option :print-success false)
(set-logic QF_LIA)

(declare-const r3c3 Int)
(declare-const r3c2 Int)
(declare-const r3c1 Int)
(declare-const r2c3 Int)
(declare-const r2c2 Int)
(declare-const r2c1 Int)
(declare-const r1c3 Int)
(declare-const r1c2 Int)
(declare-const r1c1 Int)
(declare-const r3c3tor3c2 Bool)
(declare-const r3c2tor3c3 Bool)
(declare-const r3c3tor2c3 Bool)
(declare-const r2c3tor3c3 Bool)
(declare-const r3c1tor3c2 Bool)
(declare-const r3c2tor3c1 Bool)
(declare-const r3c1tor2c1 Bool)
(declare-const r2c1tor3c1 Bool)
(declare-const r2c2tor3c2 Bool)
(declare-const r3c2tor2c2 Bool)
(declare-const r2c2tor2c3 Bool)
(declare-const r2c3tor2c2 Bool)
(declare-const r2c2tor2c1 Bool)
(declare-const r2c1tor2c2 Bool)
(declare-const r2c2tor1c2 Bool)
(declare-const r1c2tor2c2 Bool)
(declare-const r1c3tor2c3 Bool)
(declare-const r2c3tor1c3 Bool)
(declare-const r1c3tor1c2 Bool)
(declare-const r1c2tor1c3 Bool)
(declare-const r1c1tor2c1 Bool)
(declare-const r2c1tor1c1 Bool)
(declare-const r1c1tor1c2 Bool)
(declare-const r1c2tor1c1 Bool)
(declare-const r3c3_size Int)
(declare-const r3c2_size Int)
(declare-const r3c1_size Int)
(declare-const r2c3_size Int)
(declare-const r2c2_size Int)
(declare-const r2c1_size Int)
(declare-const r1c3_size Int)
(declare-const r1c2_size Int)
(declare-const r1c1_size Int)
(declare-const r3c3_count Int)
(declare-const r3c2_count Int)
(declare-const r3c1_count Int)
(declare-const r2c3_count Int)
(declare-const r2c2_count Int)
(declare-const r2c1_count Int)
(declare-const r1c3_count Int)
(declare-const r1c2_count Int)
(declare-const r1c1_count Int)
(declare-const r3c3_root Int)
(declare-const r3c2_root Int)
(declare-const r3c1_root Int)
(declare-const r2c3_root Int)
(declare-const r2c2_root Int)
(declare-const r2c1_root Int)
(declare-const r1c3_root Int)
(declare-const r1c2_root Int)
(declare-const r1c1_root Int)
(declare-const r3c3_num Int)
(declare-const r3c2_num Int)
(declare-const r3c1_num Int)
(declare-const r2c3_num Int)
(declare-const r2c2_num Int)
(declare-const r2c1_num Int)
(declare-const r1c3_num Int)
(declare-const r1c2_num Int)
(declare-const r1c1_num Int)
(declare-const r3c3_sum Int)
(declare-const r3c2_sum Int)
(declare-const r3c1_sum Int)
(declare-const r2c3_sum Int)
(declare-const r2c2_sum Int)
(declare-const r2c1_sum Int)
(declare-const r1c3_sum Int)
(declare-const r1c2_sum Int)
(declare-const r1c1_sum Int)
(assert (not (and r3c3tor3c2 r3c2tor3c3)))
(assert (not (and r3c3tor2c3 r2c3tor3c3)))
(assert (not (and r3c1tor3c2 r3c2tor3c1)))
(assert (not (and r3c1tor2c1 r2c1tor3c1)))
(assert (not (and r2c2tor3c2 r3c2tor2c2)))
(assert (not (and r2c2tor2c3 r2c3tor2c2)))
(assert (not (and r2c2tor2c1 r2c1tor2c2)))
(assert (not (and r2c2tor1c2 r1c2tor2c2)))
(assert (not (and r1c3tor2c3 r2c3tor1c3)))
(assert (not (and r1c3tor1c2 r1c2tor1c3)))
(assert (not (and r1c1tor2c1 r2c1tor1c1)))
(assert (not (and r1c1tor1c2 r1c2tor1c1)))
(assert (=> r3c2tor3c3 (not 
 r2c3tor3c3)))
(assert (=> r2c3tor3c3 (not 
 r3c2tor3c3)))
(assert (=> r3c3tor3c2 (not (or
 r3c1tor3c2
 r2c2tor3c2))))
(assert (=> r3c1tor3c2 (not (or
 r3c3tor3c2
 r2c2tor3c2))))
(assert (=> r2c2tor3c2 (not (or
 r3c3tor3c2
 r3c1tor3c2))))
(assert (=> r3c2tor3c1 (not 
 r2c1tor3c1)))
(assert (=> r2c1tor3c1 (not 
 r3c2tor3c1)))
(assert (=> r3c3tor2c3 (not (or
 r2c2tor2c3
 r1c3tor2c3))))
(assert (=> r2c2tor2c3 (not (or
 r3c3tor2c3
 r1c3tor2c3))))
(assert (=> r1c3tor2c3 (not (or
 r3c3tor2c3
 r2c2tor2c3))))
(assert (=> r3c2tor2c2 (not (or
 r2c3tor2c2
 r2c1tor2c2
 r1c2tor2c2))))
(assert (=> r2c3tor2c2 (not (or
 r3c2tor2c2
 r2c1tor2c2
 r1c2tor2c2))))
(assert (=> r2c1tor2c2 (not (or
 r3c2tor2c2
 r2c3tor2c2
 r1c2tor2c2))))
(assert (=> r1c2tor2c2 (not (or
 r3c2tor2c2
 r2c3tor2c2
 r2c1tor2c2))))
(assert (=> r3c1tor2c1 (not (or
 r2c2tor2c1
 r1c1tor2c1))))
(assert (=> r2c2tor2c1 (not (or
 r3c1tor2c1
 r1c1tor2c1))))
(assert (=> r1c1tor2c1 (not (or
 r3c1tor2c1
 r2c2tor2c1))))
(assert (=> r2c3tor1c3 (not 
 r1c2tor1c3)))
(assert (=> r1c2tor1c3 (not 
 r2c3tor1c3)))
(assert (=> r2c2tor1c2 (not (or
 r1c3tor1c2
 r1c1tor1c2))))
(assert (=> r1c3tor1c2 (not (or
 r2c2tor1c2
 r1c1tor1c2))))
(assert (=> r1c1tor1c2 (not (or
 r2c2tor1c2
 r1c3tor1c2))))
(assert (=> r2c1tor1c1 (not 
 r1c2tor1c1)))
(assert (=> r1c2tor1c1 (not 
 r2c1tor1c1)))
(assert (= r3c3_count (+
 1
 (ite r3c3tor3c2 r3c2_count 0)
 (ite r3c3tor2c3 r2c3_count 0))))
(assert (= r3c2_count (+
 1
 (ite r3c2tor3c3 r3c3_count 0)
 (ite r3c2tor3c1 r3c1_count 0)
 (ite r3c2tor2c2 r2c2_count 0))))
(assert (= r3c1_count (+
 1
 (ite r3c1tor3c2 r3c2_count 0)
 (ite r3c1tor2c1 r2c1_count 0))))
(assert (= r2c3_count (+
 1
 (ite r2c3tor3c3 r3c3_count 0)
 (ite r2c3tor2c2 r2c2_count 0)
 (ite r2c3tor1c3 r1c3_count 0))))
(assert (= r2c2_count (+
 1
 (ite r2c2tor3c2 r3c2_count 0)
 (ite r2c2tor2c3 r2c3_count 0)
 (ite r2c2tor2c1 r2c1_count 0)
 (ite r2c2tor1c2 r1c2_count 0))))
(assert (= r2c1_count (+
 1
 (ite r2c1tor3c1 r3c1_count 0)
 (ite r2c1tor2c2 r2c2_count 0)
 (ite r2c1tor1c1 r1c1_count 0))))
(assert (= r1c3_count (+
 1
 (ite r1c3tor2c3 r2c3_count 0)
 (ite r1c3tor1c2 r1c2_count 0))))
(assert (= r1c2_count (+
 1
 (ite r1c2tor2c2 r2c2_count 0)
 (ite r1c2tor1c3 r1c3_count 0)
 (ite r1c2tor1c1 r1c1_count 0))))
(assert (= r1c1_count (+
 1
 (ite r1c1tor2c1 r2c1_count 0)
 (ite r1c1tor1c2 r1c2_count 0))))
(assert (=> (or r3c3tor3c2 r3c2tor3c3) (= r3c3_size r3c2_size)))
(assert (=> (or r3c3tor2c3 r2c3tor3c3) (= r3c3_size r2c3_size)))
(assert (=> (or r3c1tor3c2 r3c2tor3c1) (= r3c1_size r3c2_size)))
(assert (=> (or r3c1tor2c1 r2c1tor3c1) (= r3c1_size r2c1_size)))
(assert (=> (or r2c2tor3c2 r3c2tor2c2) (= r2c2_size r3c2_size)))
(assert (=> (or r2c2tor2c3 r2c3tor2c2) (= r2c2_size r2c3_size)))
(assert (=> (or r2c2tor2c1 r2c1tor2c2) (= r2c2_size r2c1_size)))
(assert (=> (or r2c2tor1c2 r1c2tor2c2) (= r2c2_size r1c2_size)))
(assert (=> (or r1c3tor2c3 r2c3tor1c3) (= r1c3_size r2c3_size)))
(assert (=> (or r1c3tor1c2 r1c2tor1c3) (= r1c3_size r1c2_size)))
(assert (=> (or r1c1tor2c1 r2c1tor1c1) (= r1c1_size r2c1_size)))
(assert (=> (or r1c1tor1c2 r1c2tor1c1) (= r1c1_size r1c2_size)))
(assert (=> (not (or
 r3c2tor3c3
 r2c3tor3c3)) (and
 (= r3c3_size r3c3_count)
 (= r3c3_root 9)
 (= r3c3_num r3c3_sum))))
(assert (=> (not (or
 r3c3tor3c2
 r3c1tor3c2
 r2c2tor3c2)) (and
 (= r3c2_size r3c2_count)
 (= r3c2_root 8)
 (= r3c2_num r3c2_sum))))
(assert (=> (not (or
 r3c2tor3c1
 r2c1tor3c1)) (and
 (= r3c1_size r3c1_count)
 (= r3c1_root 7)
 (= r3c1_num r3c1_sum))))
(assert (=> (not (or
 r3c3tor2c3
 r2c2tor2c3
 r1c3tor2c3)) (and
 (= r2c3_size r2c3_count)
 (= r2c3_root 6)
 (= r2c3_num r2c3_sum))))
(assert (=> (not (or
 r3c2tor2c2
 r2c3tor2c2
 r2c1tor2c2
 r1c2tor2c2)) (and
 (= r2c2_size r2c2_count)
 (= r2c2_root 5)
 (= r2c2_num r2c2_sum))))
(assert (=> (not (or
 r3c1tor2c1
 r2c2tor2c1
 r1c1tor2c1)) (and
 (= r2c1_size r2c1_count)
 (= r2c1_root 4)
 (= r2c1_num r2c1_sum))))
(assert (=> (not (or
 r2c3tor1c3
 r1c2tor1c3)) (and
 (= r1c3_size r1c3_count)
 (= r1c3_root 3)
 (= r1c3_num r1c3_sum))))
(assert (=> (not (or
 r2c2tor1c2
 r1c3tor1c2
 r1c1tor1c2)) (and
 (= r1c2_size r1c2_count)
 (= r1c2_root 2)
 (= r1c2_num r1c2_sum))))
(assert (=> (not (or
 r2c1tor1c1
 r1c2tor1c1)) (and
 (= r1c1_size r1c1_count)
 (= r1c1_root 1)
 (= r1c1_num r1c1_sum))))
(assert (=> (or r3c3tor3c2 r3c2tor3c3) (= r3c3_root r3c2_root)))
(assert (=> (or r3c3tor2c3 r2c3tor3c3) (= r3c3_root r2c3_root)))
(assert (=> (or r3c1tor3c2 r3c2tor3c1) (= r3c1_root r3c2_root)))
(assert (=> (or r3c1tor2c1 r2c1tor3c1) (= r3c1_root r2c1_root)))
(assert (=> (or r2c2tor3c2 r3c2tor2c2) (= r2c2_root r3c2_root)))
(assert (=> (or r2c2tor2c3 r2c3tor2c2) (= r2c2_root r2c3_root)))
(assert (=> (or r2c2tor2c1 r2c1tor2c2) (= r2c2_root r2c1_root)))
(assert (=> (or r2c2tor1c2 r1c2tor2c2) (= r2c2_root r1c2_root)))
(assert (=> (or r1c3tor2c3 r2c3tor1c3) (= r1c3_root r2c3_root)))
(assert (=> (or r1c3tor1c2 r1c2tor1c3) (= r1c3_root r1c2_root)))
(assert (=> (or r1c1tor2c1 r2c1tor1c1) (= r1c1_root r2c1_root)))
(assert (=> (or r1c1tor1c2 r1c2tor1c1) (= r1c1_root r1c2_root)))
(assert (=> (or r3c3tor3c2 r3c2tor3c3) (= r3c3_sum r3c2_sum)))
(assert (=> (or r3c3tor2c3 r2c3tor3c3) (= r3c3_sum r2c3_sum)))
(assert (=> (or r3c1tor3c2 r3c2tor3c1) (= r3c1_sum r3c2_sum)))
(assert (=> (or r3c1tor2c1 r2c1tor3c1) (= r3c1_sum r2c1_sum)))
(assert (=> (or r2c2tor3c2 r3c2tor2c2) (= r2c2_sum r3c2_sum)))
(assert (=> (or r2c2tor2c3 r2c3tor2c2) (= r2c2_sum r2c3_sum)))
(assert (=> (or r2c2tor2c1 r2c1tor2c2) (= r2c2_sum r2c1_sum)))
(assert (=> (or r2c2tor1c2 r1c2tor2c2) (= r2c2_sum r1c2_sum)))
(assert (=> (or r1c3tor2c3 r2c3tor1c3) (= r1c3_sum r2c3_sum)))
(assert (=> (or r1c3tor1c2 r1c2tor1c3) (= r1c3_sum r1c2_sum)))
(assert (=> (or r1c1tor2c1 r2c1tor1c1) (= r1c1_sum r2c1_sum)))
(assert (=> (or r1c1tor1c2 r1c2tor1c1) (= r1c1_sum r1c2_sum)))
(assert (= r3c3_num (+
 r3c3
 (ite r3c3tor3c2 r3c2_num 0)
 (ite r3c3tor2c3 r2c3_num 0))))
(assert (= r3c2_num (+
 r3c2
 (ite r3c2tor3c3 r3c3_num 0)
 (ite r3c2tor3c1 r3c1_num 0)
 (ite r3c2tor2c2 r2c2_num 0))))
(assert (= r3c1_num (+
 r3c1
 (ite r3c1tor3c2 r3c2_num 0)
 (ite r3c1tor2c1 r2c1_num 0))))
(assert (= r2c3_num (+
 r2c3
 (ite r2c3tor3c3 r3c3_num 0)
 (ite r2c3tor2c2 r2c2_num 0)
 (ite r2c3tor1c3 r1c3_num 0))))
(assert (= r2c2_num (+
 r2c2
 (ite r2c2tor3c2 r3c2_num 0)
 (ite r2c2tor2c3 r2c3_num 0)
 (ite r2c2tor2c1 r2c1_num 0)
 (ite r2c2tor1c2 r1c2_num 0))))
(assert (= r2c1_num (+
 r2c1
 (ite r2c1tor3c1 r3c1_num 0)
 (ite r2c1tor2c2 r2c2_num 0)
 (ite r2c1tor1c1 r1c1_num 0))))
(assert (= r1c3_num (+
 r1c3
 (ite r1c3tor2c3 r2c3_num 0)
 (ite r1c3tor1c2 r1c2_num 0))))
(assert (= r1c2_num (+
 r1c2
 (ite r1c2tor2c2 r2c2_num 0)
 (ite r1c2tor1c3 r1c3_num 0)
 (ite r1c2tor1c1 r1c1_num 0))))
(assert (= r1c1_num (+
 r1c1
 (ite r1c1tor2c1 r2c1_num 0)
 (ite r1c1tor1c2 r1c2_num 0))))
(assert (and
 (= r3c3_size 3)
 (= r3c2_size 3)
 (= r3c1_size 3)
 (= r2c3_size 3)
 (= r2c2_size 3)
 (= r2c1_size 3)
 (= r1c3_size 3)
 (= r1c2_size 3)
 (= r1c1_size 3)))

(check-sat)
(get-model)