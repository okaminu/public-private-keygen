package lt.okaminu.publicprivatekey

import java.math.BigInteger

import scala.io.StdIn._
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

object Main extends App {
  var mode = 0

  while (mode != 4) {
    println()
    println("== Public-Private-Key ==")
    println("Select option:")
    println("1. Generate private and public keys")
    println("2. Encrypt message with public key")
    println("3. Decrypt message with private key")
    println("4. Exit")
    mode = readInt()

    mode match {
      case 1 => generateKeys()
      case 2 => encryptMessage()
      case 3 => decryptMessage()
    }
  }

  private def decryptMessage(): Unit = {
    println("Input private key multipliedPrimes:")
    val multipliedPrimes = readLong()
    println("Input private key modularInverse:")
    val modularInverse = readLong()

    println("Input encrypted number")
    val encryptedNumber = readLong()

    val decryptedNumber = BigInteger.valueOf(encryptedNumber).pow(modularInverse.toInt).mod(BigInteger.valueOf(multipliedPrimes))

    println(s"Decrypted number is: $decryptedNumber")
  }

  private def encryptMessage(): Unit = {
    println("Input public key multipliedPrimes:")
    val multipliedPrimes = readLong()
    println("Input public key totientCoprime:")
    val totientCoprime = readLong()

    println("Input number to encrypt")
    val number = readLong()

    val encryptedNumber = BigInteger.valueOf(number).pow(totientCoprime.toInt).mod(BigInteger.valueOf(multipliedPrimes))

    println(s"Encrypted number is: $encryptedNumber")
  }

  private def generateKeys(): Unit = {
    val prime1 = primeNumber(Random.between(1, 20))
    val prime2 = primeNumber(Random.between(1, 20))

    val multipliedPrimes = prime1 * prime2
    val totient = (prime1 - 1) * (prime2 - 1)

    val totientCoprime = primeNumber(Random.between(1, totient))

    val modularInverse = BigInteger.valueOf(totientCoprime).modInverse(BigInteger.valueOf(totient))

    // my implementation of modular multiplicative inverse, but it's slower than BigInteger, so I didn't use it
//    val modularInverse = calculateModInverse(totientCoprime, totient)

    println(s"Public key: multipliedPrimes $multipliedPrimes, totientCoprime $totientCoprime")
    println(s"Private key: multipliedPrimes $multipliedPrimes, modularInverse ${modularInverse.toString}")
  }

  private def calculateModInverse(totientCoprime: Long, totient: Long) = {
    var modularInverse = 1L
    breakable {
      while (modularInverse < Long.MaxValue) {
        if ((totientCoprime * modularInverse) % totient == 1L) break
        modularInverse += 1
      }
    }
  }

  private def primeNumber(until: Long) = {
    var a = 2L
    var lastPrime = 2L
    while (a < until) {
      if (isPrime(a)) lastPrime = a
      a += 1
    }
    lastPrime
  }

  private def isPrime(number: Long): Boolean = {
    var i = 2L
    while (i * i <= number) {
      if (number % i == 0)
        return false
      i += 1
    }
    true
  }

}
