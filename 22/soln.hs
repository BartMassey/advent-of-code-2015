-- Copyright © 2015 Bart Massey

import Soln

data XPC = PC | NPC
           deriving (Eq, Show, Ord)

type Effect = (Stats, Stats) -> (Stats, Stats)

data Stats = Stats {
      statsType :: XPC,
      statsHP :: Int,
      statsDamage :: Int,
      statsArmor :: Int,
      statsMana :: Int,
      statsManaSpent :: Int }
             deriving Show

data Spell = Spell {
      spellName :: String,
      spellCost, spellDuration :: Int,
      spellEffect, spellPostEffect:: Effect }

instance Show Spell where
    show spell = "<<" ++ spellName spell ++
                   "@" ++ show (spellDuration spell) ++ ">>"

data Result = Result XPC Int
              deriving (Show, Ord, Eq)

pc :: Stats
pc = Stats {
       statsType = PC,
       statsHP = 50,
       statsDamage = 0,
       statsArmor = 0,
       statsMana = 500,
       statsManaSpent = 0 }

parseBoss :: String -> Stats
parseBoss stuff
    | ["Hit_Points:", "Damage:"] == head parse =
        let [hpstr, dstr] = last parse in
        Stats {
          statsType = NPC,
          statsHP = read hpstr,
          statsDamage = read dstr,
          statsArmor = 0,
          statsMana = 0,
          statsManaSpent = 0 }
    | otherwise = error "bad stats"
    where
      parse = transpose $ map words $ lines stuff

applyDamage :: Int -> Stats -> Stats
applyDamage damage combatant =
    combatant { statsHP =
                    statsHP combatant -
                    (1 `max` (damage - statsArmor combatant)) }

applyHealing :: Int -> Stats -> Stats
applyHealing healing combatant =
    combatant { statsHP = statsHP combatant + healing }

applyMana :: Int -> Stats -> Stats
applyMana mana combatant =
    combatant { statsMana = statsMana combatant + mana }

effectDamage :: Effect
effectDamage (attacker, defender) =
    case statsType attacker of
      NPC -> error "damage applied to PC"
      PC -> (attacker, applyDamage 4 defender)
    
effectDrain :: Effect
effectDrain (attacker, defender) =
    case statsType attacker of
      NPC -> error "drain applied to PC"
      PC -> (applyHealing 2 attacker, applyDamage 2 defender)

effectShield :: Effect
effectShield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 7 })
      PC -> (attacker { statsArmor = 7 }, defender)

effectUnshield :: Effect
effectUnshield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 0 })
      PC -> (attacker { statsArmor = 0 }, defender)

effectPoison :: Effect
effectPoison (attacker, defender) =
    case statsType attacker of
      NPC -> (applyDamage 3 attacker, defender)
      PC -> (attacker, applyDamage 3 defender)

effectRecharge :: Effect
effectRecharge (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, applyMana 101 defender)
      PC -> (applyMana 101 attacker, defender)

spellbook :: [Spell]
spellbook = [
  Spell "Magic Missile" 53 0 effectDamage id,
  Spell "Drain"         73 0 effectDrain id,
  Spell "Shield"       113 6 effectShield effectUnshield,
  Spell "Poison"       173 6 effectPoison id,
  Spell "Recharge"     229 5 effectRecharge id ]

spendMana :: Spell -> Stats -> Stats
spendMana spell caster =
    let cost = spellCost spell in
    caster {
      statsMana = statsMana caster - cost,
      statsManaSpent = statsManaSpent caster + cost }

fightSim :: [Spell] -> Stats -> Stats -> Result
fightSim effects attacker defender =
    let (newEffects, (effectedAttacker, effectedDefender)) =
            foldr applyEffect ([], (attacker, defender)) effects
    in
      if statsHP effectedAttacker <= 0 then
          case statsType effectedAttacker of
            PC -> error "PC attacker died of spell effects"
            NPC -> Result PC (statsManaSpent effectedDefender)
      else if statsHP effectedDefender <= 0 then
          case statsType effectedDefender of
            PC -> error "PC defender died of spell effects"
            NPC -> Result PC (statsManaSpent effectedAttacker)
      else
          fightRound newEffects effectedAttacker effectedDefender
    where
      applyEffect spell (filteredEffects, combatants)
          | spellDuration spell < 0 = error "overqueued spell"
          | spellDuration spell == 0 =
              (filteredEffects,
               spellPostEffect spell combatants)
          | otherwise =
              (decrementDuration spell : filteredEffects,
               spellEffect spell combatants)
          where
            decrementDuration s =
                s { spellDuration = spellDuration s - 1 }

fightRound :: [Spell] -> Stats -> Stats -> Result
fightRound effects attacker defender =
    case statsType attacker of
      PC ->
          case spellChoices of
            [] -> Result NPC (statsManaSpent attacker)
            cs -> minimum $ map magicalAttack cs
          where
            magicalAttack spell =
                let chargedAttacker = spendMana spell attacker in
                case spellDuration spell of
                  0 ->
                    let (effectedAttacker, effectedDefender) =
                          spellEffect spell (chargedAttacker, defender) in
                    if statsHP effectedDefender <= 0
                    then Result PC (statsManaSpent effectedAttacker)
                    else fightSim effects effectedDefender effectedAttacker
                  _ -> 
                    let addedEffects = spell : effects in
                    fightSim addedEffects defender chargedAttacker
            spellChoices =
                sortBy (comparing spellCost) $ filter okSpell spellbook
                where
                  okSpell spell =
                      spellCost spell <= statsMana attacker &&
                      all (\s -> spellName spell /= spellName s) effects
      NPC ->
          let effectedDefender =
                applyDamage (statsDamage attacker) defender in
          if statsHP effectedDefender <= 0
          then Result NPC (statsManaSpent effectedDefender)
          else fightSim effects effectedDefender attacker

solna :: String -> IO ()
solna stuff = do
  let boss = parseBoss stuff
  print $ fightSim [] pc boss

solnb :: String -> IO ()
solnb stuff = do
  print stuff

main :: IO ()
main = makeMain solna solnb
