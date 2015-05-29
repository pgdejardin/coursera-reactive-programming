package kvstore

import akka.actor.{Actor, ActorRef}

object Arbiter {

  /**
   * This message contains all replicas currently known to the arbiter, including the primary.
   */
  case class Replicas(replicas: Set[ActorRef])

  case object Join

  case object JoinedPrimary

  case object JoinedSecondary

}

class Arbiter extends Actor {

  import Arbiter._

  var leader: Option[ActorRef] = None
  var replicas = Set.empty[ActorRef]

  def receive = {
    case Join =>
      if (leader.isEmpty) {
        leader = Some(sender())
        replicas += sender
        sender ! JoinedPrimary
      } else {
        replicas += sender
        sender ! JoinedSecondary
      }
      leader foreach (_ ! Replicas(replicas))
  }

}
