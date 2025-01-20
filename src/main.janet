(import spork/sh :as sh)
(import spork/json :as json)
(use spork/argparse)
(use ./colors)

(def argparse-params
  ["Get your todoist history"
   "limit"    {:kind :option
               :help "Limit is per request, not total. Works best if dont specify a project with children."
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
  (let [auth  (string "Authorization: Bearer " (token))
        res   (sh/exec-slurp "curl" "-s" "-X" "GET" path "-H" auth)]
    (json/decode res)))

(defn get-time-str [days]
  (let [days-in-seconds (* 60 60 24)
        t (- (os/time) (* days days-in-seconds))
        {:year y :month m :month-day d} (os/date t)]
    (string y "-" (+ 1 m) "-" (+ 1 d) "T00:00:00")))

(defn str->date [str]
  (let [date '{:main (* (<- :d+) "-" (<- :d+) "-" (<- :d+))}]
    (peg/match date str)))

# TODO: monday, tuesday etc.
(defn display-date [time str]
  ```
  from something like:
  2023-01-26T08:35:02.000000Z
  to
  26/01/2023
  or
  yesterday
  ```
  (defn is-day? [day y m d]
    (let [t (- time (match day
                      :yesterday (* 60 60 24)
                      :today     0))
          {:year yy :month mm :month-day dd} (os/date t)]
      (and (= y yy) (= m (+ 1 mm)) (= d (+ 1 dd)))))

  (let [m (str->date str)]
    (if (nil? m) ""
      (let [[y mm d] m
            ynumber (-> y   (int/s64) (int/to-number))
            mnumber (-> mm  (int/s64) (int/to-number))
            dnumber (-> d   (int/s64) (int/to-number))]
        (cond
          (is-day? :yesterday ynumber mnumber dnumber) "yesterday "
          (is-day? :today     ynumber mnumber dnumber) "today     "
          # Default:
          (string d "/" mm "/" y))))))

(defn get-project-id [name]
  "Gets project id based on name"
  (let [id (as?-> (get-todoist "https://api.todoist.com/rest/v2/projects") _
                  (filter (fn [p] (= name (get p "name"))) _)
                  (get _ 0)
                  (get _ "id"))]
    (if (nil? id)
      (error (string "No such project with name: " name))
      id)))

(defn get-project-ids [name]
  "Gets project ids based on name

  Returns the project id for the given name,
  and all its children as a map
  [{:name \"name\"
    :id  \"123\"}]
  or fails if name is not matching
  "
  (defn project->result [p]
    {:name (get p "name")
     :id   (get p "id")})

  (defn get-children [parent-id projects &opt accum]
    (default accum @[])
    (loop [p :in projects
             :when (= parent-id (get p "parent_id"))]
      (array/push accum (project->result p))
      # And its children
      (let [ids (get-children (get p "id") projects)]
        (when (not= (length ids) 0)
          (array/push accum ;ids))))
    accum)

  (let [projects    (get-todoist "https://api.todoist.com/rest/v2/projects") 
        org-project (as?-> projects _
                      (filter (fn [p] (= name (get p "name"))) _)
                      (get _ 0))
        org-id      (get org-project "id")]
    (when (nil? org-id)
      (error (string "No such project with name: " name)))

    (get-children org-id projects @[(project->result org-project)])))


(defn merge-items-and-project-ids [{:items items :project-ids project-ids}]
  (defn merge-project [item]
    (let [project-id (get item "project_id")
          project    (as?-> project-ids _
                       (filter |(= project-id (get $ :id)) _)
                       (get _ 0))]
      (if (nil? project)
        (error (string "No such project with id when merging: " project-id))
        (merge item {:project-name (get project :name)}))))
  (map merge-project items))

(defn make-result [{"items" items "projects" projects}]
  {:items items
   :project-ids (map (fn [p] {:name (get p "name") :id (get p "id")}) projects)})

# If we have multiple project ids, we need to multiple requests
# or else we only need to do one request, but then we have no way of knowing
# all of the projects that are in the result..
(defn get-completed [args]
 (let [days          (-> (get args "days" "0") (int/s64) (int/to-number))
       limit         (string "limit=" (get args "limit" "200"))
       since         (if (= days 0) "" (string "&since=" (get-time-str days)))
       project-name  (get args "project")
       project-ids   (if (or (= project-name "") (nil? project-name)) nil
                         (get-project-ids project-name))]
       # project       (if (nil? project-id) "" (string "&project_id=" project-id))]
   (if (or (= project-name "") (nil? project-name))

     # No project name, just do one request
     # How to handle project names now...?
     (-> (string "https://api.todoist.com/sync/v9/completed/get_all?" limit since)
         (get-todoist)
         (make-result)
         (merge-items-and-project-ids))

     # We have a project name, do multiple requests
     (let [project-ids (get-project-ids project-name)]
       (var items @[])
       (loop [{:id id :name name} :in project-ids]
         (def res (-> (string "https://api.todoist.com/sync/v9/completed/get_all?" limit since (string "&project_id=" id))
                      (get-todoist)
                      (get "items" @[])))
         (array/push items ;res))
       (merge-items-and-project-ids {:items items
                                     :project-ids project-ids})))))

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

(defn display [items]
  (let [project-name-length   (->> (map |(get $ :project-name) items)
                                   (map length)
                                   (apply max)
                                   (+ 2))
        now                   (os/time)]
    (loop [i :in items]
      (let [completed     (->> (get i "completed_at") (display-date now))
            project-name  (string-with-width project-name-length "#" (get i :project-name "no content"))
            content       (string-with-width 50 (get i "content" "no content"))]
        (print (color :dark-gray completed) " "
               (color :cyan project-name) " "
               content)))
    (print (string "Total: " (length items)))))

(defn create-time [year month day]
  "Manually creating time from 1970
  Not accurate and very hacky.."
  (let [y (-> (- year 1970) (* 60 60 24 365))
        m (-> (- month 1)   (* 60 60 24 30))
        d (-> (- day 1)     (* 60 60 24))]
    (+ y m d)))

(defn sort-by-completed-at [item]
  "Since we dont have a good way of sorting by date, we create a `time`
   manually by adding up all the days
   the number of days since the item was completed"
  (let [c (get item "completed_at")
        [year month day] (map |(-> (int/u64 $) (int/to-number)) (str->date c))]
    (create-time year month day)))

(defn main [&]
  (setup)
  (let [args (argparse ;argparse-params)]
    (when (nil? args)
      (os/exit 1))
    (->> (get-completed args)
        (sort-by sort-by-completed-at)
        (display))))

(comment
  (setup)

  (let [y -10]
    (* y 60 60 24 365))

  (let [args {"days" "7"
              "project" "personal"}]
    (-> (get-completed args)))

  (get-project-id "chores")

  (get-project-ids "personal"))

