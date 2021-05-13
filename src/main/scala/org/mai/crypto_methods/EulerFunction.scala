package org.mai.crypto_methods

import Euclidean.gcd

class EulerFunction {

  def phi(n: Int): Int = (1 to n).count(gcd(n, _) == 1)

}
