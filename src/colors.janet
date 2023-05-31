# Ansi terminal colors
#
# Compliant with: https://no-color.org/
#
# TODO: move to a separate repo
# Taken from: https://github.com/janet-lang/janet/blob/master/examples/colors.janet
(def- colormap
  {:black 30
   :bg-black 40
   :red 31
   :bg-red 41
   :green 32
   :bg-green 42
   :yellow 33
   :bg-yellow 43
   :blue 34
   :bg-blue 44
   :magenta 35
   :gray 37
   :bg-magenta 45
   :cyan 36
   :bg-cyan 46
   :white 37
   :bg-white 47
   :bright-black 90
   :bg-bright-black 100
   :bright-red 91
   :bg-bright-red 101
   :dark-gray 90
   :bright-green 92
   :bg-bright-green 102
   :bright-yellow 93
   :bg-bright-yellow 103
   :bright-blue 94
   :bg-bright-blue 104
   :bright-magenta 95
   :bg-bright-magenta 105
   :bright-cyan 96
   :bg-bright-cyan 106
   :bright-white 97
   :bg-bright-white 107})

(defn no-color []
  "Checks if you have set the NO_COLOR environment variable.
  Also stores the result in a dynamic variable for future use."
  (let [no-color-dyn (dyn :color/no-color)]
    (cond
      (not (nil? no-color-dyn)) no-color-dyn
      # If :color/no-color has not been set,
      # we check env for env and set it
      (let [no-color-env (os/getenv "NO_COLOR")
            no-color     (and (string? no-color-env) (not= no-color-env ""))]
         (setdyn :color/no-color no-color)
         no-color))))

(defn color
  "Take a string made by concatenating xs and colorize it for an ANSI terminal."
  [c & xs]
  (let [code (get colormap c)]
    (cond
      (no-color) (string ;xs)
      (not code) (error (string "color " c " unknown"))
      (string "\e[" code "m" ;xs "\e[0m"))))
