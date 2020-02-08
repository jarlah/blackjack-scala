package game

import scala.annotation.tailrec
import scala.util.Random

sealed trait Suit
case object Heart extends Suit
case object Diamond extends Suit
case object Spade extends Suit
case object Club extends Suit

sealed abstract class Rank(val value: Int) {
  val isAce: Boolean = this == Ace
}
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
  def dealCard: (Card, Deck) = {
    val topCard = cards.head
    (topCard, copy(cards.filter(_ != topCard)))
  }
}

object Deck {
  val allCards: Seq[Card] =
    for {
      suit <- List(Heart, Diamond, Spade, Club)
      rank <- List(King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Ace)
    } yield Card(suit, rank)

  def shuffle(random: Random): Deck =
    Deck(random.shuffle(allCards))
}

case class Hand(cards: Seq[Card]) {
  private val winningValue = 21
  val value: Int = cards.map(_.rank.value).sum
  val containsAce: Boolean = cards.exists(_.rank.isAce)
  val specialValue: Int = if (containsAce) value + 10 else value
  val isBlackJack: Boolean = value == winningValue || specialValue == winningValue
  val isBust: Boolean = value > winningValue
  val bestValue: Int =  List(value, specialValue).filter(_ <= winningValue).maxOption.getOrElse(0)
  def winsOver(otherHand: Hand): Boolean = bestValue > otherHand.bestValue
  def showCards (dealer: Boolean = false): String =
    if (dealer) s"${cards.head.rank.value} X"
    else cards.map(c => c.rank.value).mkString(", ")
  def addCard(card: Card): Hand = copy(cards = cards :+ card)
}

object Dealer {
  def dealHand(deck: Deck): (Hand, Deck) = {
    val (firstCard, deck1) = deck.dealCard
    val (secondCard, deck2) = deck1.dealCard
    (Hand(Seq(firstCard, secondCard)), deck2)
  }

  def dealHands(deck: Deck): (Hand, Hand, Deck) = {
    val (firstHand, deck1) = Dealer.dealHand(deck)
    val (secondHand, deck2) = Dealer.dealHand(deck1)
    (firstHand, secondHand, deck2)
  }
}

case class GameState(userCredit: Int) {
  val moneyLeft: Boolean = userCredit > 0
}

object Game extends App {

  println("Welcome to BlackJack. Press any key to start playing.")
  readLine("")

  gameLoop(GameState(100), new Random())

  @tailrec
  def gameLoop(gameState: GameState, random: Random): Unit = {
    val deck = Deck.shuffle(random)
    val bet = readInt(s"Please enter bet (credit: ${gameState.userCredit}): ")
    val (playerHand, dealerHand, modifiedDeck) = Dealer.dealHands(deck)
    val playerWon = roundLoop(playerHand, dealerHand, modifiedDeck, stand = false)
    val newState = gameState.copy(userCredit = gameState.userCredit + (if (playerWon) bet else -bet))
    println(s"======= Game Summary =======")
    println(s"Start-Credit: [ ${gameState.userCredit} ],  End-Credit: [ ${newState.userCredit} ]")
    println()
    if (newState.moneyLeft && continue("Do you want to continue? ")) {
      gameLoop(newState, random)
    } else if (!newState.moneyLeft)
      println("You have no money left")
    else
      println("Exiting")
  }

  @tailrec
  def roundLoop(playerHand: Hand, dealerHand: Hand, deck: Deck, stand: Boolean): Boolean = {
    val summary = roundSummary(playerHand, dealerHand)(_)
    if (playerHand.isBust) { // player has hit and is bust
      summary(false)
    } else if (stand) { // player is standing
      if (dealerHand.isBust || playerHand.winsOver(dealerHand)) {
        summary(true)
      } else {
        summary(false)
      }
    } else {
      val (newPlayerHand, newDealerHand, newDeck, newStand) =
        hitOrStand(playerHand, dealerHand, deck)
      roundLoop(newPlayerHand, newDealerHand, newDeck, newStand)
    }
  }

  def hitOrStand(playerHand: Hand, dealerHand: Hand, deck: Deck): (Hand, Hand, Deck, Boolean) = {
    var newDeck = deck
    var newDealerHand = dealerHand
    var newPlayerHand = playerHand
    var newStand = false
    showCards(playerHand, dealerHand)
    if ("s".equals(getHitOrStand)) {
      newStand = true
      while (newDealerHand.value < 17) {
        newDeck.dealCard match {
          case (card, modifiedDeck) =>
            newDealerHand = newDealerHand.addCard(card)
            newDeck = modifiedDeck
        }
      }
    } else {
      val (card, modifiedDeck) = newDeck.dealCard
      newPlayerHand = newPlayerHand.addCard(card)
      newDeck = modifiedDeck
    }
    (newPlayerHand, newDealerHand, newDeck, newStand)
  }

  def showCards(playerHand: Hand, dealerHand: Hand, dealer: Boolean = true): Unit = {
    println(s"Dealer hand: ${dealerHand.showCards(dealer)}")
    println(s"Player hand: ${playerHand.showCards()}")
  }

  def roundSummary(player: Hand, dealer: Hand)(won: Boolean): Boolean = {
    println(s"*** You ${if (won) "win" else "lose!"} ***")
    showCards(player, dealer, dealer = false)
    won
  }

  def getHitOrStand: String = {
    var input = ""
    // Loop till user enters either "s" or "h"
    while (!List("s", "h").contains(input)) {
      input = readLine("Hit or Stand? Enter one of (H,h,S,s): ").toLowerCase
    }
    input
  }
}
