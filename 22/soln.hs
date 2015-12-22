-- Copyright © 2015 Bart Massey

import Soln

data XPC = PC | NPC deriving (Eq, Show, Ord)

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
    let (effects', (attacker', defender')) =
            foldr applyEffect ([], (attacker, defender)) effects
    in
      if statsHP attacker' <= 0 then
          case statsType attacker' of
            PC -> Result NPC (statsManaSpent attacker)
            NPC -> Result PC (statsManaSpent defender)
      else if statsHP defender' <= 0 then
          case statsType defender' of
            PC -> Result NPC (statsManaSpent defender)
            NPC -> Result PC (statsManaSpent attacker)
      else
          fightRound effects' attacker' defender'
    where
      applyEffect spell (newEffects, combatants)
          | spellDuration spell < 0 = error "overqueued spell"
          | spellDuration spell == 0 =
              (newEffects, spellPostEffect spell combatants)
          | otherwise =
              (decrementDuration spell : newEffects,
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
                    let attacker' = spendMana spell attacker in
                    case spellDuration spell of
                      0 ->
                        if statsHP defender' <= 0
                        then Result PC (statsManaSpent attacker'')
                        else fightSim effects defender' attacker''
                        where
                          (attacker'', defender') =
                              spellEffect spell (attacker', defender)
                      _ -> 
                        fightSim effects' defender attacker'
                        where
                          effects' = spell : effects
          where
            spellChoices =
                sortBy (comparing spellCost) $ filter okSpell spellbook
                where
                  okSpell spell =
                      spellCost spell <= statsMana attacker &&
                      all (\s -> spellName spell /= spellName s) effects
      NPC ->
          let defender' =
                  applyDamage (statsDamage attacker) defender
          in
          if statsHP defender' <= 0
          then Result NPC (statsManaSpent defender')
          else fightSim effects defender' attacker

solna :: String -> IO ()
solna stuff = do
  let boss = parseBoss stuff
  print $ fightSim [] pc boss

solnb :: String -> IO ()
solnb stuff = do
  print stuff

main :: IO ()
main = makeMain solna solnb
