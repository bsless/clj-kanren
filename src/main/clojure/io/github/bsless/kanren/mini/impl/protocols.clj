(ns io.github.bsless.kanren.mini.impl.protocols)

(defprotocol IWalkTerm
  "Ad-hoc implementation of tree walk for various data structures.
  Used in the implementation of reification."
  (-walk-term [v f]))

(defprotocol IReifyTerm
  "Behavior for reifying a specific term in substitution.
  The no-op case returns the substitution unchanged."
  (-reify-term [v s]))

