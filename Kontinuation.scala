trait Frame {
  def subsumes(that: Frame): Boolean
  def isCallFrame: Boolean
}

trait Kontinuation {
  def subsumes(that: Kontinuation): Boolean
  def getFrame: Frame
}
