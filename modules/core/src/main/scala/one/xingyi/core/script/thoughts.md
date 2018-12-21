Let us examine the 'person' microservice that returns information about a person

Firstly there are three models that vary across time
M1: Person(name/line1/line2)
M2: Person(name) -> Address(line1/line2)
M3: Person(name) -* Address(line1/line2

The server is using M3. The goal is that it will ship the data in the format M3.

example.com/person returns:

{
  "person": {
    "endpoints":    {
      "new": {
        "link":  "http://example.com/person/id/<id>",
        "verbs": ["get", "put"]
      },
      "id":  {
        "link":  "http://example.com/person/id/<id>",
        "verbs": ["get", "put"]
      }
    },
    "capabilities": {
      "entities": {
        "person":  ["name/string", "address/address", "addresslist/list[address]", "line1/string", "line2/string"],
        "address": ["line1/string", "line2/string"]
      },
      "code":     {
        "person_name.person_line1.person_line2":                                                                {
          "java":       "http://example.com/code/java/..hash..",
          "scala":      "http://example.com/code/scala/..hash..",
          "javascript": "http://example.com/code/javascript/..hash.."
        },
        "person_name.person_address_list.address_line1.address_line2":                                          {
          "java":       "http://example.com/code/java/..hash..",
          "scala":      "http://example.com/code/scala/..hash..",
          "javascript": "http://example.com/code/javascript/..hash.."
        },
        "person_name.person_line1.person_line2.person_address.person_address_list.address_line1.address_line2": {
          "java":       "http://example.com/code/java/..hash..",
          "scala":      "http://example.com/code/scala/..hash..",
          "javascript": "http://example.com/code/javascript/..hash.."
        }
      }
    }
  }
}
with media-type application/xingyi.navigation

So from this we can work out how to make a new one, how to get one
We have a list of the capabilities (really just for humans
The code isn't needed either... it's really just there for testing and stuff
This should have a cache timeout which the client should take note of. So this controls the stale cache time for this page

When we get a person:
1: we send an 'accept'.
    We should be able to allow multiple accepts... so that we can write so that we can deal with m1 only servers for example... An example mime from a m1 client would be
    application/xingyi.person_name.person_line1.person_line2 
    
2: The server works out if it has code that satisfies. If not 415 (unsupported media code)
3: It returns the data. Now... the content type I think should be the media type. The first line of the text is the url of the code. The rest is the content
4: The client gets the code (cashing is obviously a good thing!)
