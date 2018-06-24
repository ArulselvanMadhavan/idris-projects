module TestStore

import DataStore

testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty

listItems : DataStore schema -> List (SchemaType schema)
listItems x with (storeView x)
  listItems x | SNil = []
  listItems (addToStore value store) | (SAdd rec) = value :: listItems store | rec

filterKeys : (test : SchemaType val_schema -> Bool) -> DataStore (SString .+. val_schema) -> List String
filterKeys test input with (storeView input)
  filterKeys test input | SNil = []
  filterKeys test (addToStore (key, value) store) | (SAdd rec) = if test value
                                                          then key :: (filterKeys test store | rec)
                                                          else filterKeys test store | rec

testStore' : DataStore (SString .+. SInt)
testStore' = addToStore ("First", 1) $
             addToStore ("Second", 2) $
             empty

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues store with (storeView store)
  getValues store | SNil = []
  getValues (addToStore (_, v) oldStore) | (SAdd rec) = v :: (getValues oldStore)
