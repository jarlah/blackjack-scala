package game

import scala.annotation.tailrec
import scala.util.Random

sealed trait Suit
case object Heart extends Suit
case object Diamond extends Suit
case object Spade extends Suit
case object Club extends Suit

sealed abstract class Rank(val value: Int)
case object King extends Rank(10)
case object Queen extends Rank(10)
case object Jack extends Rank(10)
case object Ten extends Rank(10)
case object Nine extends Rank(9)
case object Eight extends Rank(8)
case object Seven extends Rank(7)
case object Six extends Rank(6)
case object Five extends Rank(5)
case object Four extends Rank(4)
case object Three extends Rank(3)
case object Two extends Rank(2)
case object Ace extends Rank(1)

case class Card(suit: Suit, rank: Rank)

case class Deck(cards: Seq[Card]) {
  // Its impossible to deal all cards, because dealer or player would be bust before that happens
  // Therefore we use cards.head instead of cards.headOption
  def dealCard: (Card, Deck) = (cards.head, copy(cards.tail))
}

object Deck {
  def shuffle(shuffle: Seq[Card] => Seq[Card]): Deck =
    Deck(shuffle(
      for {
        suit <- List(Heart, Diamond, Spade, Club)
        rank <- List(King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Ace)
      } yield Card(suit, rank)
    ))
}

case class Hand(cards: Seq[Card]) {
  private val winningValue = 21
  val value: Int = cards.map(_.rank.value).sum
  val containsAce: Boolean = cards.exists(_.rank == Ace)
  val specialValue: Int = if (containsAce) value + 10 else value
  val isBlackJack: Boolean = value == winningValue || specialValue == winningValue
  val isBust: Boolean = value > winningValue
  val bestValue: Int =  List(value, specialValue).filter(_ <= winningValue).maxOption.getOrElse(0)
  def winsOver(otherHand: Hand): Boolean = bestValue > otherHand.bestValue
  def showCards (dealer: Boolean = false): String = if (dealer) s"${cards.head.rank.value} X" else cards.map(c => c.rank.value).mkString(", ")
  def addCard(card: Card): Hand = copy(cards = cards :+ card)
}

object Dealer {
  def dealHands(deck: Deck): (Hand, Hand, Deck) = {
    val (firstCard, deck1) = deck.dealCard
    val (secondCard, deck2) = deck1.dealCard
    val (thirdCard, deck3) = deck2.dealCard
    val (fourthCard, deck4) = deck3.dealCard
    (Hand(Seq(firstCard, secondCard)), Hand(Seq(thirdCard, fourthCard)), deck4)
  }
}

case class GameState(credit: Int)

object Game extends App {
  val continue = () => "y".equals(getAnswer("Do you wan to continue", List("y", "n")))
  val stand = () => "s".equals(getAnswer("Hit or Stand", List("h", "s")))
  val randomShuffle = (cards: Seq[Card]) => Random.shuffle(cards)
  val bidSupplier = (currentCredit: Int) => {
    var bet = readInt(s"Please enter bet (credit: ${currentCredit}): ")
    while(bet > currentCredit || bet == 0) {
      println("Bad input. Try again")
      bet = readInt(s"Please enter bet (credit: ${currentCredit}): ")
    }
    bet
  }

  println("Welcome to BlackJack. Press any key to start playing.")
  readLine("")
  gameLoop(GameState(100), randomShuffle, bidSupplier, continue, stand)

  @tailrec
  def gameLoop(gameState: GameState, shuffleFn: Seq[Card] => Seq[Card], bidSupplier: Int => Int, shouldContinue: () => Boolean, shouldStand: () => Boolean): Unit = {
    val deck = Deck.shuffle(shuffleFn)
    val bet = bidSupplier.apply(gameState.credit)
    val (playerHand, dealerHand, newDeck) = Dealer.dealHands(deck)
    val playerWon = roundLoop(playerHand, dealerHand, newDeck, stand = false, shouldStand)
    val newState = gameState.copy(credit = gameState.credit + (if (playerWon) bet else -bet))
    println(s"======= Game Summary =======")
    println(s"Start-Credit: [ ${gameState.credit} ],  End-Credit: [ ${newState.credit} ]")
    println()
    if (newState.credit > 0 && shouldContinue()) {
      gameLoop(newState, shuffleFn, bidSupplier, shouldContinue, shouldStand)
    } else if (newState.credit <= 0) {
      println("You have no money left")
    } else {
      println("Exiting")
    }
  }

  @tailrec
  def roundLoop(playerHand: Hand, dealerHand: Hand, deck: Deck, stand: Boolean, shouldStand: () => Boolean): Boolean = {
    if (playerHand.isBust) { // player has hit and is bust
      roundSummary(playerHand, dealerHand, won = false)
    } else if (stand) { // player is standing
      roundSummary(playerHand, dealerHand, dealerHand.isBust || playerHand.winsOver(dealerHand))
    } else {
      val (newPlayerHand, newDealerHand, newDeck, newStand) =
        hitOrStand(playerHand, dealerHand, deck, shouldStand)
      roundLoop(newPlayerHand, newDealerHand, newDeck, newStand, shouldStand)
    }
  }

  @tailrec
  def dealerTryToWin(dealerHand: Hand, deck: Deck): (Hand, Deck) = {
    if (dealerHand.value >= 18) {
      (dealerHand, deck)
    } else {
      deck.dealCard match {
        case (card, modifiedDeck) =>
          dealerTryToWin(dealerHand.addCard(card), modifiedDeck)
      }
    }
  }

  def hitOrStand(playerHand: Hand, dealerHand: Hand, deck: Deck, shouldStand: () => Boolean): (Hand, Hand, Deck, Boolean) = {
    showCards(playerHand, dealerHand)
    if (shouldStand()) {
      val (newDealerHand, newDeck) = dealerTryToWin(dealerHand, deck)
      (playerHand, newDealerHand, newDeck, true)
    } else {
      val (card, newDeck) = deck.dealCard
      (playerHand.addCard(card), dealerHand, newDeck, false)
    }
  }

  def showCards(playerHand: Hand, dealerHand: Hand, dealer: Boolean = true): Unit = {
    println(s"Dealer hand: ${dealerHand.showCards(dealer)}")
    println(s"Player hand: ${playerHand.showCards()}")
  }

  def roundSummary(player: Hand, dealer: Hand, won: Boolean): Boolean = {
    println(s"*** You ${if (won) "win" else "lose!"} ***")
    showCards(player, dealer, dealer = false)
    won
  }
}
