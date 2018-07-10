module Ex12_2_3

record Votes where
  constructor MkVotes
  upvotes : Integer
  downvotes : Integer

record Article where
  constructor MkArticle
  title : String
  url : String
  score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

addUpvote : Article -> Article
addUpvote = record { score->upvotes $= (+1) }

addDownVote : Article -> Article
addDownVote = record { score->downvotes $= (+1) }

getScore : Article -> Integer
getScore (MkArticle title url score) = (upvotes score) - (downvotes score)

badSite : Article
badSite = MkArticle "Bad page" "blah bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good page" "good_url" (MkVotes 101 7)
