(import testament :prefix "" :exit true)
(import ../src/main :as main)

(deftest string-with-width
  (let [s1 (main/string-with-width 10 "hello janet!!!")
        s2 (main/string-with-width 10 "hello")]
    (is (= "hello jane" s1))
    (is (= "hello     " s2))))

(deftest display-date
  (let [s1 (main/display-date "2023-01-22T08:35:02.000000Z")
        s2 (main/display-date "2021-12-01T08:35")
        yesterday (main/get-time-str 1)
        s3 (main/display-date yesterday)]
    (is (= "22/01/2023" s1))
    (is (= "01/12/2021" s2))
    (is (= "yesterday " s3))))

(run-tests!)
