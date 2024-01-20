module type Pass = sig
  val pass_fn : AnalysisManager.t -> Mr.mfn -> unit
end
