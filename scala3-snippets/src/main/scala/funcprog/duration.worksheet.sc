import java.util.concurrent.TimeUnit

case class Duration(value: Int, unit: TimeUnit)

object Duration:

  object Syntax:
    import scala.language.implicitConversions

    implicit class HasSeconds(n: Int) {
      def seconds: Duration = Duration(n, TimeUnit.SECONDS)
    }
  end Syntax

end Duration

// extension (n: Int) def seconds = Duration(n, TimeUnit.SECONDS)
import Duration.Syntax.*
5.seconds

List(1, 2, 3).sorted
