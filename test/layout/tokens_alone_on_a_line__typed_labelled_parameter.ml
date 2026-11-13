let component
  ~(submit_feedback :
      (Protocol.Submit_feedback.Query.t
       -> Protocol.Submit_feedback.Response.t Or_error.t Effect.t)
        Bonsai.t)
  (local_ graph)
  =
  submit_feedback, graph
;;
