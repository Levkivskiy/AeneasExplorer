package scorex.core.mainviews

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent
}
