import objsets.*

// def asSet(tweets: TweetSet): Set[Tweet] =
//   var res = Set[Tweet]()
//   tweets.foreach(res += _)
//   res

// def size(set: TweetSet): Int = asSet(set).size

val a = Tweet("a", "a body", 20)
val b = Tweet("b", "b body", 20)
val c = Tweet("c", "c body", 7)
val d = Tweet("d", "d body", 9)
val e = Tweet("e", "e body", 9)

val set1 = Empty().incl(a).incl(b).incl(c)
val set2 = Empty().incl(d).incl(e)

val set3 = set1.union(set2).asSet.size

set1.filter(_.retweets > 10).asSet.size
