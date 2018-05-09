{
def number(x: Int): Int =
    if(even(x)) 100 else 200;

  def even(x: Int): Boolean =
    if (x == 0) true else odd(x-1);

  def odd(x: Int): Boolean =
    if (x == 0) false else even(x-1);

  even(1);
  def boolean(b: Int): Int =
    if(b==1) false else true;

  boolean(a)
}
