-- Copyright © 2015 Bart Massey

-- Got help debugging from this solution
--   https://www.reddit.com/r/adventofcode/comments/
--     3xspyl/day_22_solutions/cy7l25a
-- although it appears to be wrong on my input: claims
-- to spend 1269 mana but actually spends 1469.

import qualified Data.Set as S
import System.Environment
import Text.Printf

import Soln

-- | Us vs them.
data XPC = PC | NPC
           deriving (Eq, Show, Ord)

-- | An effect applies to both attacker and defender.
type Effect = (Stats, Stats) -> (Stats, Stats)

-- | Description of XPC.
data Stats = Stats {
      statsType :: XPC,
      statsHP :: Int,
      statsDamage :: Int,
      statsArmor :: Int,
      statsMana :: Int,
      statsManaSpent :: Int }
             deriving (Ord, Eq, Show)

-- | Description of Spell.
data Spell = Spell {
      spellName :: String,
      spellCost, spellDuration :: Int,
      spellEffect, spellPostEffect:: Effect }

-- | Spells are distinguished only by name.
instance Ord Spell where
    compare = compare `on` spellName

-- | Spells are distinguished only by name.
instance Eq Spell where
    (==) = (==) `on` spellName

-- | For debugging.
instance Show Spell where
    show spell = "<<" ++ spellName spell ++
                   "@" ++ show (spellDuration spell) ++ ">>"

-- | The PC.
pc :: Stats
pc = Stats {
       statsType = PC,
       statsHP = 50,
       statsDamage = 0,
       statsArmor = 0,
       statsMana = 500,
       statsManaSpent = 0 }

-- | Read the NPC description.
parseBoss :: String -> Stats
parseBoss stuff =
    case map words $ lines stuff of
      [[ "Hit", "Points:", hpstr ],
       [ "Damage:", dstr ]] ->
        Stats {
          statsType = NPC,
          statsHP = read hpstr,
          statsDamage = read dstr,
          statsArmor = 0,
          statsMana = 0,
          statsManaSpent = 0 }
      _ -> error "bad stats"

-- | Apply damage adjusted for armor, with minimum 1 damage.
applyDamage :: Int -> Stats -> Stats
applyDamage damage combatant =
    combatant { statsHP =
                    statsHP combatant -
                    (1 `max` (damage - statsArmor combatant)) }

-- | Heal.
applyHealing :: Int -> Stats -> Stats
applyHealing healing combatant =
    combatant { statsHP = statsHP combatant + healing }

-- | Increase mana.
applyMana :: Int -> Stats -> Stats
applyMana mana combatant =
    combatant { statsMana = statsMana combatant + mana }

-- | Damage spell effect.
effectDamage :: Effect
effectDamage (attacker, defender) =
    case statsType attacker of
      NPC -> error "damage applied to PC"
      PC -> (attacker, applyDamage 4 defender)
    
-- | Drain spell effect.
effectDrain :: Effect
effectDrain (attacker, defender) =
    case statsType attacker of
      NPC -> error "drain applied to PC"
      PC -> (applyHealing 2 attacker, applyDamage 2 defender)

-- | Shield spell effect.
effectShield :: Effect
effectShield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 7 })
      PC -> (attacker { statsArmor = 7 }, defender)

-- | Shield spell post-effect.
effectUnshield :: Effect
effectUnshield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 0 })
      PC -> (attacker { statsArmor = 0 }, defender)

-- | Poison spell effect.
effectPoison :: Effect
effectPoison (attacker, defender) =
    case statsType attacker of
      NPC -> (applyDamage 3 attacker, defender)
      PC -> (attacker, applyDamage 3 defender)

-- | Recharge spell effect.
effectRecharge :: Effect
effectRecharge (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, applyMana 101 defender)
      PC -> (applyMana 101 attacker, defender)

-- | Treat "hard mode" as a spell effect.
effectHardMode :: Effect
effectHardMode (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender)
      PC -> (applyDamage 1 attacker, defender)

-- | The spell table.
spellbook :: [Spell]
spellbook = [
  Spell "Magic Missile" 53 0 effectDamage id,
  Spell "Drain"         73 0 effectDrain id,
  Spell "Shield"       113 6 effectShield effectUnshield,
  Spell "Poison"       173 6 effectPoison id,
  Spell "Recharge"     229 5 effectRecharge id ]

-- | Decrease caster mana and record its expenditure.
spendMana :: Spell -> Stats -> Stats
spendMana spell caster =
    let cost = spellCost spell
        newMana = statsMana caster - cost
    in
      if newMana < 0
      then error "overspent mana"
      else caster {
             statsMana = newMana,
             statsManaSpent = statsManaSpent caster + cost }

-- | Tick off the clock on a spell.
decrementDuration :: Spell -> Spell
decrementDuration spell
    | duration <= 0 = error "illegal spell duration decrement"
    | otherwise = spell { spellDuration = duration - 1 }
    where
      duration = spellDuration spell

-- | Result carries who died and how much mana was spent
-- by the PC.
data Result = Result XPC Int
              deriving (Show, Ord, Eq)

-- | The current state of combat. The left combatant
-- is the attacker, the right the defender.
data FightState = FightState {
      fightCombatants :: (Stats, Stats),
      fightEffects :: [Spell] }
                  deriving (Ord, Eq, Show)

-- | A 'Result' when the fight is over, a 'FightState'
-- while still ongoing. Used extensively with the
-- 'Either' monad.
type FightStatus = Either Result FightState

-- | See if either side died. If both sides
-- die simultaneously, the defender wins.
checkForDead :: FightState -> FightStatus
checkForDead state = do
  when (dead attacker)
       (Left (Result (statsType defender) finalMana))
  when (dead defender)
       (Left (Result (statsType attacker) finalMana))
  Right state
  where
    (attacker, defender) = fightCombatants state
    dead combatant = statsHP combatant <= 0
    finalMana
      | statsType attacker == PC = statsManaSpent attacker
      | statsType defender == PC = statsManaSpent defender
      | otherwise = error "no PC"

-- | Apply a spell effect to a state, possibly
-- killing someone in the process.
runEffect :: Effect -> FightState -> FightStatus
runEffect effect state = do
  let state' =
          state { fightCombatants =
                      effect (fightCombatants state) }
  checkForDead state'

-- | Run all the currently active effects, expiring those
-- that need expiring. The order here is crucial beyond
-- belief.
runEffects :: FightState -> FightStatus
runEffects state = do
  instantState <- foldM (runSpell spellEffect) state instants
  spellState <- foldM (runSpell spellEffect) instantState running
  doneState <- foldM (runSpell spellPostEffect) spellState done
  Right $ doneState { fightEffects = instants ++ running' }
  where
    (instants, running) =
        partition ((== (-1)) . spellDuration) $ fightEffects state
    (done, running') =
        partition ((== 0) . spellDuration) $ map decrementDuration running
    runSpell effectSelector curState spell =
        runEffect (effectSelector spell) curState

-- | Spend the mana cost of a spell.
spendSpell :: Spell -> FightState -> FightState
spendSpell spell state =
  let (attacker, defender) = fightCombatants state
      spentAttacker = spendMana spell attacker
  in
  case spentAttacker of
    a | statsType a /= PC -> error "NPC spell attack"
    a | statsManaSpent a < 0 -> error "PC attack overspent mana"
    a -> state { fightCombatants = (a, defender) }

-- | Exchange attacker and defender.
changeSides :: FightState -> FightState
changeSides state =
    let (attacker, defender) = fightCombatants state in
    state { fightCombatants = (defender, attacker) }

-- | Run the PC's attack with a specific spell.
runPCAttack :: Spell -> FightState -> FightStatus
runPCAttack spell state =
  let spentState = spendSpell spell state in
  case spellDuration spell of
    d | d < 0 -> error "pc attack with infinite duration spell"
    0 ->
      let effectedCombatants =
              spellEffect spell $ fightCombatants spentState
          effectedState = state { fightCombatants = effectedCombatants }
      in
      checkForDead $ changeSides effectedState
    _ ->
      let effects = fightEffects spentState in
      Right $ changeSides $ spentState { fightEffects = spell : effects }

-- | Run the NPC's attack.
runNPCAttack :: FightState -> FightStatus
runNPCAttack state = do
  let (attacker, defender) = fightCombatants state
  let effectedDefender = applyDamage (statsDamage attacker) defender
  let finalState = state { fightCombatants = (effectedDefender, attacker) }
  checkForDead finalState

-- | Find the spells in the spellbook that are not currently
-- active and not too expensive.
availableSpells :: FightState -> [Spell]
availableSpells state
  | statsType attacker == NPC =
      error "NPC spell attack"
  | otherwise =
      filter availableSpell spellbook
  where
    (attacker, _) = fightCombatants state
    availableSpell spell =
        statsMana attacker >= spellCost spell &&
        all (/= spell) (fightEffects state)

-- | Run a combat turn, returning a set of possible outcomes
-- depending on PC spell selection.  The NPC always does the
-- same thing.
runCombat :: FightState -> S.Set FightStatus
runCombat state =
    let (attacker, _) = fightCombatants state in
    case statsType attacker of
      PC ->
          case availableSpells state of
            [] -> S.singleton $ Left $
                    Result NPC (statsManaSpent attacker)
            spells -> S.fromList $ map (flip runPCAttack state) spells
      NPC ->
          S.singleton $ runNPCAttack state

-- | Fight round simulator.
runRound :: S.Set FightState -> S.Set FightStatus
runRound states =
    S.unions $ map combat $ S.toList states
    where
      combat state =
          either (S.singleton . Left) runCombat $ runEffects state

-- | Cheezy calculation of PC mana spent so far depends
-- on NPC always spending 0 mana.
manaSpent :: FightState -> Int
manaSpent state =
    uncurry (max `on` statsManaSpent) $ fightCombatants state

-- | Run a fight round on each of a set of statuses
-- and combine the results. Ensure that rounds terminate
-- early if the mana spent in the starting state is not
-- less than that found in the best solution so far.
processRound :: S.Set FightStatus -> S.Set FightStatus
processRound statuses =
    done `S.union` runRound alive
    where
      best = minimum $ S.toList statuses
      (running, done) = S.partition isRight statuses
      alive = S.filter okMana $ S.map getState running
      okMana :: FightState -> Bool
      okMana state =
          case best of
            Right  _ -> True
            Left (Result NPC _) -> True
            Left (Result PC mana) -> manaSpent state < mana
      getState :: FightStatus -> FightState
      getState (Left _) = error "running finished state"
      getState (Right s) = s

-- | Run a fight until all possible best outcomes have been
-- achieved, then return a best outcome.
runFight :: FightState -> FightStatus
runFight state0 =
    let soln = find (isLeft . maximum . S.toList) $
                 iterate processRound $ S.singleton (Right state0)
    in
    case soln of
      Nothing -> error "fight ended early"
      Just ss -> minimum $ S.toList ss

-- | Run a round with the PC picking a specific spell.
tryRound :: FightStatus -> Spell -> IO FightStatus
tryRound status spell = do
    printf "starting round: %s\n" (spellName spell)
    print status
    let status' = do
          state <- status
          let (attacker, _) = fightCombatants state
          when (statsType attacker /= PC)
               (error "flipped try round")
          effectedState <- runEffects state
          unless (spell `elem` availableSpells effectedState)
                 (Left $ Result NPC (statsManaSpent attacker))
          runPCAttack spell effectedState
    printf "\n"
    print status'
    printf "\n"
    return $ do
      state <- status'
      effectedState <- runEffects state
      runNPCAttack effectedState

-- | A sample solution that gives PC victory. See the comment
-- at the top of this file for where it came from.
trySpells :: [Spell]
trySpells =
    map lookupSpell $ [
      "Shield",
      "Recharge",
      "Poison",
      "Shield",
      "Recharge",
      "Poison",
      "Shield" ] ++
      replicate 6 "Magic Missile"
    where
      lookupSpell name = fromJust $ find ((== name) . spellName) spellbook

-- | Run a directed fight with the given sequence of PC
-- spellcasts.  Display the outcome.
simulate :: [Spell] -> String -> IO ()
simulate spells stuff = do
  printf "expected mana spent: %d\n" (sum $ map spellCost spells)
  let boss = parseBoss stuff
  let state0 = FightState (pc, boss) []
  status <- foldM tryRound (Right state0) spells
  print status
  
-- | Show an outcome in a more readable form.
summarizeFight :: FightStatus -> String
summarizeFight (Right state) =
    printf "? %s %d"
           (show $ statsType $ fst $ fightCombatants state)
           (manaSpent state)
summarizeFight (Left (Result who mana)) =
    printf "! %s %d" (show who) mana

-- | Show a trace of rounds of fighting.
trace :: Int -> String -> IO ()
trace n stuff = do
  let boss = parseBoss stuff
  putStr $ summarizeRounds $ take n $
    iterate processRound $ S.singleton $ Right $
    FightState (pc, boss) []
  where
    summarizeRounds rounds =
        unlines $ map (unlines . map summarizeFight . S.toList) rounds

-- | Strategy: Run the fight, show the result.
solna :: String -> IO ()
solna stuff = do
  let boss = parseBoss stuff
  putStrLn $ summarizeFight $ runFight $ FightState (pc, boss) []

-- | Strategy: Make "hard mode" just be a spell that runs
-- forever.  Cast it at the start of the fight.
solnb :: String -> IO ()
solnb stuff = do
  let boss = parseBoss stuff
  putStrLn $ summarizeFight $ runFight $ FightState (pc, boss) [hardMode]
  where
    hardMode = Spell "HardMode" 0 (-1) effectHardMode id

-- | When called with the argument '?s', runs a simulation.
-- When called with the argument '?t#', runs a trace.
main :: IO ()
main = do
  stuff <- readFile "input.txt"
  args <- getArgs
  case length args of
    0 -> solna stuff
    _ -> case args of
           ["-"] -> solnb stuff
           ["?s"] -> simulate trySpells stuff
           ['?' : 't' : ns] -> trace (read ns) stuff
           _ -> error "bad args"
