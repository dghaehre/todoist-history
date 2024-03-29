(import testament :prefix "" :exit true)
(import ../src/main :as main)

(deftest string-with-width
  (let [s1 (main/string-with-width 10 "hello janet!!!")
        s2 (main/string-with-width 10 "hello")]
    (is (= "hello jane" s1))
    (is (= "hello     " s2))))

(deftest display-date
  (let [now (os/time)
        s1 (main/display-date now "2023-01-22T08:35:02.000000Z")
        s2 (main/display-date now "2021-12-01T08:35")
        yesterday (main/get-time-str 1)
        today     (main/get-time-str 0)
        s3 (main/display-date now yesterday)
        s4 (main/display-date now today)]
    (is (= "22/01/2023" s1))
    (is (= "01/12/2021" s2))
    (is (= "yesterday " s3))
    (is (= "today     " s4))))

(deftest create-time
  # Not accurate
  (is (= 1608940800 (main/create-time 2021 1 8))))

(deftest test-sort
  (let [items @[{:name "a" "completed_at" "2021-01-01T08:35:02.000000Z"}
                {:name "b" "completed_at" "2021-01-02T08:35:02.000000Z"}
                {:name "c" "completed_at" "2021-01-03T08:35:02.000000Z"}]
        [one two three] (-> (sort-by main/sort-by-completed-at items) (reverse))]
    (is (= "c" (get one :name)))
    (is (= "b" (get two :name)))
    (is (= "a" (get three :name)))))

(run-tests!)
