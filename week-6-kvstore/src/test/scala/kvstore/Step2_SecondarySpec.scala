package kvstore

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import kvstore.Arbiter.{Join, JoinedSecondary}
import org.scalactic.ConversionCheckedTripleEquals
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

import scala.concurrent.duration._

class Step2_SecondarySpec extends TestKit(ActorSystem("Step2SecondarySpec"))
with FunSuiteLike
with BeforeAndAfterAll
with Matchers
with ConversionCheckedTripleEquals
with ImplicitSender
with Tools {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  test("case1: Secondary (in isolation) should properly register itself to the provided Arbiter") {
    val arbiter = TestProbe()
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case1-secondary")

    arbiter.expectMsg(Join)
  }

  test("case2: Secondary (in isolation) must handle Snapshots") {
    import Replicator._

    val arbiter = TestProbe()
    val replicator = TestProbe()
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case2-secondary")
    val client = session(secondary)

    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)

    client.get("k1") should ===(None)

    replicator.send(secondary, Snapshot("k1", None, 0L))
    replicator.expectMsg(SnapshotAck("k1", 0L))
    client.get("k1") should ===(None)

    replicator.send(secondary, Snapshot("k1", Some("v1"), 1L))
    replicator.expectMsg(SnapshotAck("k1", 1L))
    client.get("k1") should ===(Some("v1"))

    replicator.send(secondary, Snapshot("k1", None, 2L))
    replicator.expectMsg(SnapshotAck("k1", 2L))
    client.get("k1") should ===(None)
  }

  test("case3: Secondary should drop and immediately ack snapshots with older sequence numbers") {
    import Replicator._

    val arbiter = TestProbe()
    val replicator = TestProbe()
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case3-secondary")
    val client = session(secondary)

    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)

    client.get("k1") should ===(None)

    replicator.send(secondary, Snapshot("k1", Some("v1"), 0L))
    replicator.expectMsg(SnapshotAck("k1", 0L))
    client.get("k1") should ===(Some("v1"))

    replicator.send(secondary, Snapshot("k1", None, 0L))
    replicator.expectMsg(SnapshotAck("k1", 0L))
    client.get("k1") should ===(Some("v1"))

    replicator.send(secondary, Snapshot("k1", Some("v2"), 1L))
    replicator.expectMsg(SnapshotAck("k1", 1L))
    client.get("k1") should ===(Some("v2"))

    replicator.send(secondary, Snapshot("k1", None, 0L))
    replicator.expectMsg(SnapshotAck("k1", 0L))
    client.get("k1") should ===(Some("v2"))
  }

  test("case4: Secondary should drop snapshots with future sequence numbers") {
    import Replicator._

    val arbiter = TestProbe()
    val replicator = TestProbe()
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case4-secondary")
    val client = session(secondary)

    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)

    client.get("k1") should ===(None)

    replicator.send(secondary, Snapshot("k1", Some("v1"), 1L))
    replicator.expectNoMsg(300.milliseconds)
    client.get("k1") should ===(None)

    replicator.send(secondary, Snapshot("k1", Some("v2"), 0L))
    replicator.expectMsg(SnapshotAck("k1", 0L))
    client.get("k1") should ===(Some("v2"))
  }


}
