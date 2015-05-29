package kvstore

import akka.actor._
import kvstore.Arbiter._
import kvstore.Replica.{Get, GetResult, Insert, Remove}
import kvstore.Replicator.Replicate

object Replica {

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))

  sealed trait Operation {
    def key: String

    def id: Long
  }

  sealed trait OperationReply

  case class Insert(key: String, value: String, id: Long) extends Operation

  case class Remove(key: String, id: Long) extends Operation

  case class Get(key: String, id: Long) extends Operation

  case class OperationAck(id: Long) extends OperationReply

  case class OperationFailed(id: Long) extends OperationReply

  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Get(key, id) => sender() ! GetResult(key, None, id)
    case Insert(key, value, id) =>
    case Remove(key, id) =>
    case Replicas(rep) =>
      val replicas = replicasDifference(rep)
      replicators --= replicas
    case Terminated(_) =>
      persistence = context.watch(context.actorOf(persistenceProps))
  }
  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case _ =>
  }
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  arbiter ! Join
  var persistence: ActorRef = context.watch(context.actorOf(persistenceProps))
  var sequencer = 0L

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  def replicasDifference(rep: Set[ActorRef]): Set[ActorRef] = {
    var actorRefToRefMap = Map.empty[ActorRef, ActorRef]
    rep foreach { replica =>
      if (secondaries.contains(replica)) {
        actorRefToRefMap += replica -> secondaries(replica)
        secondaries -= replica
      } else if (replica != self) {
        val newReplica = context.actorOf(Replicator.props(replica))
        replicators += newReplica
        actorRefToRefMap += replica -> newReplica
        kv foreach { elem =>
          newReplica ! Replicate(elem._1, Some(elem._2), -1)
        }
      }
    }
    val result = secondaries.values.toSet
    result foreach {
      _ ! PoisonPill
    }
    secondaries = actorRefToRefMap
    result
  }

}

