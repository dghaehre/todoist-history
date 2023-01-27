(declare-project
  :name "todoist-history"
  :description ```A cli that outputs your todoist history ```
  :dependencies ["https://github.com/janet-lang/spork"
                 "https://github.com/pyrmont/testament"]
  :version "0.0.1")

(declare-executable
  :name "todoist-history"
  :entry "./src/main.janet"
  :install true)
