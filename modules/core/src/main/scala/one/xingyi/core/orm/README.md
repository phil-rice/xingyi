# Question 
Given these two:
* https://blog.codinghorror.com/object-relational-mapping-is-the-vietnam-of-computer-science/
* http://blogs.tedneward.com/post/the-vietnam-of-computer-science/

Why are we bothering with an ORM?

Also note that some Databases are getting better at this. For example Postgres with some of it's customer json extensions can dodge the N+1 problem (as per https://www.graphile.org/postgraphile/)

# Answer:

We have some of the following:
* We need to get data out of the database for such purposes as GraphQl, or because we
are working with a legacy database 
* We want to avoid the N+1 problem: a naive implementation will preform terribly when getting large volumnes of data out
* We want to turn the data into a block of json rather than an object, and the json is just a report on what is there

In these situations we are not significantly using the 'O' of ORM: we just want to get the data out so that we can
do some operations on it. When legacy modernising a typical need is to 'make the data visible in an api', or to typically applying a function to it to transform it, and/or checking that it matches some constraints.

In these cases the orm here lets us deal with the N+1 without much extra overhead in a declarative fashion.

# Using it

## Way 1: roll your own OrmMaker

This is actually a really good approach if you have a simple schema / objects.  You define the relationships between the data
the data is pulled into memory efficiently, and then you are responsible for composing the objects. you do need to understand how the
data is stored, but looking at the tests will show you that the OrmMaker is quite straightforward to implement

```scala
  implicit val maker: OrmMaker[Person] = { main =>
    data: Map[OrmEntity, List[List[Any]]] =>
      import OrmMaker._

      val eMap = employer.toMap(data, implicit list => Employer(str(0)))
      val aMap = address.toOneToManyMap(data, main, implicit list => Address(str(0)))
      val phoneMap = phone.toOneToManyMap(data, main, implicit list => Phone(str(0)))
      val emailMap = email.toMap(data, implicit list => str(0))
      data(main).map { implicit oneRow =>
        Person(str(0),
          employer.getData(main, eMap, oneRow),
          address.getData(main, aMap, oneRow),
          phone.getData(main, phoneMap, oneRow),
          email.getData(main, emailMap, oneRow))
      }.toStream
  }
```
This is the orm maker. 
* emap is 'from employee id to Employees'
* amap is from employee id to the list of addresses for that employee 
* phoneMap from the employee id to the list of phones for that employee
* emailMap from the employee id to the email for that employee 
Then we walk over the data and compose the person
  
It is awkward to create, but extremely efficient in terms of N+1

## Schema

This is for when the layout of the data is programmed through another declarative layer like a schema. This is much more complicated to understand, but
reasonably efficient. 

Here the schema has to follow some rules (i.e have type classes defined for it)

```scala
trait SchemaMapKey[Schema[_]] {
  def childKey[T](t: Schema[T]): String // The main object might not have a key, but the children will
  def children[T](t: Schema[T]): List[Schema[_]]
}
trait ToAliasAndFieldTypes[Schema[_]] {def apply[T](s: Schema[T]): List[AliasAndFieldTypes[Schema, T]]}
```
* The first is about the 'names' and children in the schema. It allows json to be made with the correct names and structure
* The second is about how to map the item to the database
    * Note that it is a list of AliasAndFieldTypes and not just one
    * In some schemas we can define  multiple places that the data can be extracted from, and also sometimes the data in the json is scattered across multiple fields in the same table
    
# Possible improvements

* At the moment we have the situation where we read every thing into memory then construct data.
       * We can use a merge sort approach 
       * this would be a huge performance improvement!
* We could let the user tell us (or perhaps do stats) and work out if it will be quicker to cache some of the tables.
* We might want to just have ids in the second temporary table. 
        * I don't think this will actually save anything though, because I suspect the database engine might well construct this table anyway
* We want more in the way stored procedures: 
        *  'create stored procedures' when we deploy the software (once)  
        *  'úse named stored procedures' as we access the code
* We might want to code up the 'bundling of ids'

