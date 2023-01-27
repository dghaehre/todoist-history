(import sh)
(use spork/argparse)
(use ./colors)
(import json)

(def argparse-params
  ["Get your todoist history"
   "limit"    {:kind :option
               :help "limit"
               :short "l"}
   "days"     {:kind :option
               :help "days"
               :short "d"}
   "project"  {:kind :option
               :help "project"
               :short "p"}])

(defn setup []
  (let [path  (string (os/getenv "HOME") "/.todoist.token")
        token (-> (slurp path) (string/trim))]
    (setdyn :todoist/token token)))

(defn token []
  (dyn :todoist/token))

(defn get-todoist [path]
  (let [auth (string "Authorization: Bearer " (token))
        res (sh/$< curl -s -X GET ,path  -H ,auth)]
    (json/decode res)))

(defn get-time-str [days]
  (let [days-in-seconds (* 60 60 24)
        t (- (os/time) (* days days-in-seconds))
        {:year y :month m :month-day d} (os/date t)]
    (string y "-" (+ 1 m) "-" (+ 1 d) "T00:00:00")))

# TODO: monday, tuesday etc.
(defn display-date [str]
  ```
  from something like:
  2023-01-26T08:35:02.000000Z
  to
  26/01/2023
  or
  yesterday
  ```

  (defn yesterday? [y m d]
    (let [y (-> y (int/s64) (int/to-number))
          m (-> m (int/s64) (int/to-number))
          d (-> d (int/s64) (int/to-number))
          t (- (os/time) (* 60 60 24))
          {:year yy :month mm :month-day dd} (os/date t)]
      (and (= y yy) (= m (+ 1 mm)) (= d (+ 1 dd)))))

  (let [date '{:main (* (<- :d+) "-" (<- :d+) "-" (<- :d+))}
        m          (peg/match date str)]
    (if (nil? m) ""
      (let [[y m d] m]
        (cond
          (yesterday? y m d) "yesterday "
          # Default:
          (string d "/" m "/" y))))))

(defn get-project-id [name]
  "Gets project id based on name"
  (let [id (as?-> (get-todoist "https://api.todoist.com/rest/v2/projects") _
                  (filter (fn [p] (= name (get p "name"))) _)
                  (get _ 0)
                  (get _ "id"))]
    (if (nil? id)
      (do
        (print "No such project with name: " name)
        (os/exit 1))
      id)))

(defn get-completed [args]
  (let [days          (-> (get args "days" "0") (int/s64) (int/to-number))
        limit         (string "limit=" (get args "limit" "200"))
        since         (if (= days 0) "" (string "&since=" (get-time-str days)))
        project-name  (get args "project")
        project-id    (if (or (= project-name "") (nil? project-name)) nil
                          (get-project-id project-name))
        project       (if (nil? project-id) "" (string "&project_id=" project-id))]
    (-> (string "https://api.todoist.com/sync/v9/completed/get_all?" limit since project)
        (get-todoist))))

(defn string-with-width [width & xs]
  (let [s (string ;xs)
        l (length s)]
    (if (> l width)
      (string/slice s 0 width)
      (string s (string/repeat " " (- width l))))))

(defn attach-project [items projects project-flag]
  (defn attach [i]
    (let [name (as-> (get i "project_id") _
                     (get projects _)
                     (get _ "name"))]
      (put i :project-name name)
      i))
  (defn attach-flag [i]
    (put i :project-name project-flag)
    i)
  (if (nil? project-flag)
    (map attach items)
    (map attach-flag items)))

(defn display [data args]
  (let [items                 (get data "items")
        projects              (get data "projects")
        project-flag          (get args "project")
        project-name-length   (if (nil? project-flag) 10 (+ 1 (length project-flag)))]
    (loop [i :in (-> (reverse items) (attach-project projects project-flag))]
      (let [completed     (->> (get i "completed_at") (display-date))
            project-name  (string-with-width project-name-length "#" (get i :project-name "no content"))
            content       (string-with-width 50 (get i "content" "no content"))]
        (print (color :dark-gray completed) " "
               (color :cyan project-name) " "
               content)))
    (print (string "Total: " (length items)))))

(defn main [&]
  (setup)
  (let [args (argparse ;argparse-params)]
    (when (nil? args)
      (os/exit 1))
    (-> (get-completed args)
        (display args))))
