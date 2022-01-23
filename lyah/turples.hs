-- Turples are similat to lists by storing values into a single value but:
-- values limit is known and the the type is determined by the number and type of tha components.
-- can contain a combination of values (not homogenenous like lists)
-- Turples should be used when the number of components in a piece of data is known
--
paired = [(3,2),(6,5)]

actors = [("Christopher Wlaken", "US", 43), ("Josepeh Weefle", "KE", 34)]

ages = (43,6)

firstElement = fst ages --get first 
secondElement = snd ages -- get second

--zip: gets two lists and joins them into one list by putting matching positional elements into pairs 
-- useful when for instance we need to traverse two lists simultenously
--
list1 = [1,2,3,4,5]
list2 = [9,8,7,6,5]
result = zip list1 list2

version = ["meta", "beta", "alpha","mega"]
numbers = [4,5,7,2]
joined = zip numbers version

