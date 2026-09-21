-- This program generates samples of all categories, with all data bits set to
-- 0 and 1.

import           Control.Monad     (forM_)
import           Data.Map          as Map
import           Data.Text.IO      as T

import           Asterix.Coding
import           Asterix.Generated (manifest)

import           Common            (sampleRecords, hexlify)

main :: IO ()
main = forM_ (Map.assocs $ latestEditionsBasic manifest) $ \(cat, (ed, uap)) -> do
    let ast = GAsterixBasic cat ed uap
    forM_ (sampleRecords ast) $ \(_cat, ((name, _sch), (r1, r2))) -> do
        dump cat name r1
        dump cat name r2
  where
    dump :: VInt -> Maybe VText -> URecord -> IO ()
    dump cat mName r =
        let records = [(mName, r)]
            bld = datablockBuilder cat (fmap snd records)
            db = UDatablock bld records
        in T.putStrLn $ hexlify $ builderToByteStringSlow $ sbData $ unparse db

