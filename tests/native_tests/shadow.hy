

(defn test-shadow-addition []
  "NATIVE: test shadow addition"
  (let [[x +]]
    (assert (= (x) 0))
    (assert (= (x 1 2 3 4) 10))
    (assert (= (x 1 2 3 4 5) 15))))


(defn test-shadow-subtraction []
  "NATIVE: test shadow subtraction"
  (let [[x -]]
    assert (x)))
