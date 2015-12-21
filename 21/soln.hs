-- Copyright © 2015 Bart Massey

import Soln

data ItemType = Weapon | Armor | Ring
              deriving (Eq, Show)

data Item = Item {
  itemType :: ItemType,
  itemName :: String,
  itemCost :: Int,
  itemDamage :: Int,
  itemArmor :: Int }
  deriving Show

data XPC = PC | NPC deriving (Eq, Show)

data Stats = Stats {
  statsType :: XPC,
  statsHP :: Int,
  statsDamage :: Int,
  statsArmor :: Int,
  statsCost :: Int,
  statsGear :: [String] }
  deriving Show

parseStats :: String -> Stats
parseStats stuff
    | ["Hit_Points:", "Damage:", "Armor:"] == head parse =
        let [hpstr, dstr, astr] = last parse in
        Stats NPC (read hpstr) (read dstr) (read astr) 0 []
    | otherwise = error "bad stats"
    where
      parse = transpose $ map words $ lines stuff

fightSimDumb :: Stats -> Stats -> XPC
fightSimDumb npc pc =
    attack pc npc
    where
      attack attacker defender =
          let dhp = calcDamage in
          if dhp <= 0
          then statsType attacker
          else attack (defender {statsHP = dhp}) attacker
          where
            calcDamage = (statsHP defender) -
              (1 `max` (statsDamage attacker - statsArmor defender))

items :: [Item]
items = [
  --           Weapons:      Cost  Damage  Armor
  Item  Weapon "Dagger"        8     4       0,
  Item  Weapon "Shortsword"   10     5       0,
  Item  Weapon "Warhammer"    25     6       0,
  Item  Weapon "Longsword"    40     7       0,
  Item  Weapon "Greataxe"     74     8       0,

  --           Armor:        Cost  Damage  Armor
  Item  Armor  "Leather"      13     0       1,
  Item  Armor  "Chainmail"    31     0       2,
  Item  Armor  "Splintmail"   53     0       3,
  Item  Armor  "Bandedmail"   75     0       4,
  Item  Armor  "Platemail"   102     0       5,

  --           Rings:        Cost  Damage  Armor
  Item  Ring   "Damage +1"    25     1       0,
  Item  Ring   "Damage +2"    50     2       0,
  Item  Ring   "Damage +3"   100     3       0,
  Item  Ring   "Defense +1"   20     0       1,
  Item  Ring   "Defense +2"   40     0       2,
  Item  Ring   "Defense +3"   80     0       3 ]

equippedPCs :: [Stats]
equippedPCs =
    map outfit gearChoices
    where
      weapons = filter ((== Weapon) . itemType) items
      armors = filter ((== Armor) . itemType) items
      rings = filter ((== Ring) . itemType) items
      ringCombos = map (:[]) rings ++ ringPairs
                   where
                     ringPairs = do
                       (r1 : rs) <- tails rings
                       r2 <- rs
                       return [r1, r2]
      gearChoices = do
        weapon <- weapons
        armor <- [] : map (:[]) armors
        ring <- [] : ringCombos
        return $ weapon : (armor ++ ring)
      outfit gear =
          foldr applyGear nakedPlayer gear
          where
            nakedPlayer = Stats PC 100 0 0 0 []
            applyGear item player =
                player {
                  statsDamage = statsDamage player + itemDamage item,
                  statsArmor = statsArmor player + itemArmor item,
                  statsCost = statsCost player + itemCost item,
                  statsGear = itemName item : statsGear player }

soln :: (Stats -> Int) -> XPC -> String -> IO ()
soln sf target stuff = do
  let boss = parseStats stuff
  case find ((target ==) . fightSimDumb boss) pcs of
    Nothing -> error "no cromulent equipment"
    Just pc -> print $ statsCost pc
  where
    pcs = sortBy (comparing sf) equippedPCs

solna :: String -> IO ()
solna stuff =
  soln statsCost PC stuff

solnb :: String -> IO ()
solnb stuff = do
  soln (negate . statsCost) NPC stuff

main :: IO ()
main = makeMain solna solnb
