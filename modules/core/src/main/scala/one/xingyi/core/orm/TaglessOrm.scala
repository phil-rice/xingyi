package one.xingyi.core.orm


/** The journey of data seems to be
 *
 * Put into temporary tables
 * Read into memory
 * Transform into a 'thing.
 * Could be a domain object, but usually just seem to want to transform it into list/map/primitive
 * The OrmKeys story is about how to turn it list/map/primitive
 *
 * Now we tried the arrays story, and it looks too hard for now. Basically it's a great future optimisation, but too brittle
 * at this stage. The arrays story says 'really efficient/read straight from cursor with no intermediate into the list/map/primitive
 * recognising that the list/map/primitive has a different (configurable) structure.
 *
 * So can I do something around Tagless Interpreters to walk the object graph in the raw data. Reasons for doing this
 * including debugging/tracing/documentation/understanding
 * */
class TaglessOrm {

}
