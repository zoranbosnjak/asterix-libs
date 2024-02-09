-- | Application specific asterix processing.
-- We are explicit about category, edition, item names... (checked at compile time).

module TestSpecific where

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
--import Test.Tasty.HUnit

-- Examples --

{-
import Asterix

-- Basic category and expansion
type Cat000 = Cat000_1_0
type Ref000 = Ref000_1_0

-- Category with multiple UAPs with UAP selector
type Cat001 = Cat001_1_0

-- Category with multiple UAPs without UAP selector
type Cat002 = Cat002_1_0

type SacSicType = SameType
   '[ Cat000 ~> "010"
    , Cat001 ~> "010"
    ]

sacSic :: Integral a => a -> a -> SacSicType bs
sacSic sac sic = group
    ( item @"SAC" (fromIntegral sac)
   &: item @"SIC" (fromIntegral sic)
   &: nil)

-- | Example: build empty datablock
test1 :: Builder
test1 = unparse db
  where
    db :: Datablock 0 bs
    db = datablock []

-- | Example: build datablock with empty records, should be the same as empty
-- datablock, since the empty record encodes to an empty byte string.
test2 :: Builder
test2 = unparse db
  where
    db :: Datablock 0 bs
    db = datablock [r,r,r,r]

    r :: Record 0 (UapOf Cat000) bs
    r = record nil

-- | Example: build simple record
test3 :: Builder
test3 = unparse db
  where
    db :: Datablock 0 bs
    db = datablock [r]

    r :: Record 0 (UapOf Cat000) bs
    r = record
        ( item @"010" 0x0102
       &: item @"051" 0x01
       &: nil)

-- | Example: build multiple UAPs
test4 :: Builder
test4 = unparse db1 <> unparse db2
  where
    db1 :: Datablock 1 bs
    db1 = datablock [recPlot, recTrack]
      where
        recPlot :: Record 1 (UapOf @"plot" Cat001) bs
        recPlot = record
            ( item @"020" $ extended
                ( item @"TYP" 0
               &: item @"I1" 0
               &: fx
               &: nil)
           &: nil)

        recTrack :: Record 1 (UapOf @"track" Cat001) bs
        recTrack = record
            ( item @"010" 0x0102
           &: item @"020" $ extended
                ( item @"TYP" 1
               &: 0
               &: fx
               &: 0
               &: fx
               &: nil)
           &: nil)

    db2 :: Datablock 2 bs
    db2 = datablock [rec1, rec2]
      where
        rec1 = Record 2 (UapOf @"uap1" Cat002) bs
        rec1 = record
            ( item @"010" $ sacSic 0x01 0x02
           &: item @"101" 0
           &: item @"102" 0
           &: nil)

        rec2 = Record 2 (UapOf @"uap2" Cat002) bs
        rec2 = record
            ( item @"010" $ sacSic 0x01 0x02
           &: item @"202" 0
           &: nil)

-- | Example: build different variation types
test5 :: Builder
test5 = unparse db
  where
    db :: Datablock 0 bs
    db = datablock [r]

    r :: Record 0 (UapOf Cat000) bs
    r = record
        ( item @"051" 0
       &: item @"052" $ group
          ( 0
         &: 0
         &: 0
         &: 0
         &: 0
         &: 0
         &: nil)
       &: item @"053" $ extended
          ( 0
         &: fx
         &: 0
         &: fx
         &: 0
         &: fx
         &: nil)
       &: item @"054" $ extended
          ( 0
         &: fx
         &: 0
         &: fx
         &: 0
         &: nil)
       &: item @"061" $ repetitive
          [0..9]
       &: item @"062" $ repetitive
          [0..9]
       &: item @"063" $ repetitive
          [0..9]
       &: item @"071" $ explicit
          "This is explicit"
       &: item @"072" $ explicit
          (unparse re)
       &: item @"073" $ explicit
          "This is explicit"
       &: item @"091" $ compound
           ( item @"I1" 0
          &: item @"I2" 0
          &: nil)
       &: item @"092" $ compound
           ( item @"I1" 0
          &: item @"I2" 0
          &: nil)

    re :: Expansion 0 Ref000 bs
    re = expansion
        ( item @"I1" 0
       &: item @"I2" 0
       &: nil)

-- | Example: various content type creation
test6 :: Builder
test6 = unparse db
  where
    db :: Datablock 0 bs
    db = datablock [r]

    r :: Record 0 (UapOf Cat000) bs
    r = record
        ( item @"020" 0x0102 $ group
            ( item @"R" 0
           &: item @"T" 0
           &: item @"S1" $ string "ABCD"
           &: item @"S2" "ABCD"
           &: item @"S3" "1234"
           &: spare 0
           &: item @"I1" $ uinteger 10
           &: item @"I2" $ integer 10
           &: item @"Q1LAT" $ quantity @"°" 1.2
           &: item @"Q2LON" $ quantity (-3.4)
           &: item @"Q3" $ uquantity @"kt" 123.4
           &: item @"Q4" $ uquantity @"" 123.4
           &: item @"Q5" $ uquantity 12.3
           &: item @"B1" 0
           &: item @"B2" 0
           &: item @"B3" 0
           &: nil
       &: nil)

-- | Example: Dependent item creation
test7 :: Builder
test7 = unparse db
  where
    db :: Datablock 0 bs
    db = datablock [r1, r2]

    r1 :: Record 0 (UapOf Cat000) bs
    r1 = record
        ( item @"030" $ group
            ( item @"IM" 0
           &: item @"IAS" $ branch @0 $ uquantity @"NM" 123.4
           &: nil)
       &: nil)

    r2 :: Record 0 (UapOf Cat000) bs
    r2 = record
        ( item @"030" $ group
            ( item @"IM" 1
           &: item @"IAS" $ branch @1 $ uquantity @"Mach" 0.7
           &: nil)
       &: nil)

-- | Example: Non-zero spare items
test8 :: Builder
test8 = unparse db
  where
    db :: Datablock 0 bs
    db = datablock [r]

    r :: Record 0 (UapOf Cat000) bs
    r = record
        ( item @"040" $ group
            ( 1
           &: 2
           &: 3
           &: 4
           &: nil)
       &: nil)

-- | Example: basic parsing - extact SAC/SIC codes from items "010", cat [0,1]
p1 :: ByteString -> Maybe [(Int, Int)]
p1 s = undefined
  where
    dbs :: Maybe [Raw.Datablock ByteString]
    dbs = parse s

    processDatablock :: Raw.Datablock ByteString -> Maybe [(Int, Int)]
    processDatablock db = case dbCategory db of
        0 -> do
            records <- parse @(UapOf Cat000) (dbData db)
            pure $ do
                r <- records
                case (getItem @"010" r) of
                    Nothing -> empty
                    Just val -> pure
                        ( getItem @"SAC" val
                        , getItem @"SIC" val
                        )
        1 -> undefined
        {- TODO
           Datablock can contain multiple records, with different UAP each.
           Use something like
           try to parse with each UAP in turn, use the first valid and able to extract
           (r, remaining) <- parseOne s
           check record 'r' if valid
           extract required data out of the record
           proceed with the remaining
           ... create asterix helper function for this kind of processing
           (one record at the time)
        -}
        _ -> pure []

-- | Example: parse dependent item
p2 :: Item (Cat000 ~> "030") -> Either Double Double
p2 val = case (getItem @"IM" val) of
    0 -> Left $ undefined -- convert using branch 0
    1 -> Right $ undefined -- convert using branch 1

-- | Example: asterix filter (rx+tx), modify category 0, set item 010
f1 :: Int -> Int -> ByteString -> Maybe Builder
f1 sac sic s = undefined

-- | Example: convert one edition to another (same category)
f2 :: ByteString -> Maybe Builder
f2 = undefined
-}

-- Tests --

tests :: TestTree
tests = testGroup "Application specific asterix tests"
    [
    ]
