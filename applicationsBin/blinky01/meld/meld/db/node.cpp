
#include <iostream>
#include <assert.h>

#include "db/node.hpp"
#include "vm/state.hpp"
#include "utils/utils.hpp"
#include "debug/debug_handler.hpp"

using namespace db;
using namespace std;
using namespace vm;
using namespace utils;

namespace db
{

tuple_trie*
node::get_storage(const predicate* pred)
{
   simple_tuple_map::iterator it(tuples.find(pred->get_id()));

   if(it == tuples.end()) {
      //cout << "New trie for " << *pred << endl;
      tuple_trie *tr(new tuple_trie(pred));
      tuples[pred->get_id()] = tr;
      return tr;
   } else
      return it->second;
}

bool
node::add_tuple(vm::tuple *tpl, const derivation_count many, const depth_t depth)
{
   const predicate* pred(tpl->get_predicate());
   tuple_trie *tr(get_storage(pred));
  
   return tr->insert_tuple(tpl, many, depth);

}

node::delete_info
node::delete_tuple(vm::tuple *tuple, const derivation_count many, const depth_t depth)
{
   const predicate *pred(tuple->get_predicate());
   tuple_trie *tr(get_storage(pred));

   return tr->delete_tuple(tuple, many, depth);

}

agg_configuration*
node::add_agg_tuple(vm::tuple *tuple, const derivation_count many, const depth_t depth)
{
   const predicate *pred(tuple->get_predicate());
   predicate_id pred_id(pred->get_id());
   aggregate_map::iterator it(aggs.find(pred_id));
   tuple_aggregate *agg;

   if(it == aggs.end()) {
      agg = new tuple_aggregate(pred);
      aggs[pred_id] = agg;
   } else
      agg = it->second;

   return agg->add_to_set(tuple, many, depth);
}

agg_configuration*
node::remove_agg_tuple(vm::tuple *tuple, const derivation_count many, const depth_t depth)
{
   return add_agg_tuple(tuple, -many, depth);
}

simple_tuple_list
node::end_iteration(void)
{
   // generate possible aggregates
   simple_tuple_list ret;

   for(aggregate_map::iterator it(aggs.begin());
      it != aggs.end();
      ++it)
   {
      tuple_aggregate *agg(it->second);

      simple_tuple_list ls(agg->generate());

      ret.insert(ret.end(), ls.begin(), ls.end());
   }

   return ret;
}

void
node::match_predicate(const predicate_id id, tuple_vector& vec) const
{
   simple_tuple_map::const_iterator it(tuples.find(id));

   if(it == tuples.end())
      return;

   const tuple_trie *tr(it->second);

   tr->match_predicate(vec);
}

void
node::match_predicate(const predicate_id id, const match& m, tuple_vector& vec) const
{
   simple_tuple_map::const_iterator it(tuples.find(id));

   if(it == tuples.end())
      return;

   const tuple_trie *tr(it->second);

   tr->match_predicate(m, vec);
}

void
node::delete_all(const predicate*)
{
   assert(false);
}

void
node::delete_by_leaf(const predicate *pred, tuple_trie_leaf *leaf, const depth_t depth)
{
   tuple_trie *tr(get_storage(pred));

   tr->delete_by_leaf(leaf, depth);
}

void
node::delete_by_index(const predicate *pred, const match& m)
{
   tuple_trie *tr(get_storage(pred));

   tr->delete_by_index(m);

   aggregate_map::iterator it(aggs.find(pred->get_id()));

   if(it != aggs.end()) {
      tuple_aggregate *agg(it->second);
      agg->delete_by_index(m);
   }
}

size_t
node::count_total(const predicate_id id) const
{
   simple_tuple_map::const_iterator it(tuples.find(id));

   if(it == tuples.end())
      return 0;

   const tuple_trie *tr(it->second);

   return tr->size();
}

void
node::assert_end(void) const
{
   for(aggregate_map::const_iterator it(aggs.begin());
      it != aggs.end();
      ++it)
   {
      assert(it->second->no_changes());
   }
}

node::node(const node_id _id
#ifdef USERFRIENDLY
           , const node_id _trans
#endif
           ):
  id(_id),
#ifdef USERFRIENDLY
  translation(_trans), 
#endif
owner(NULL)
{
}

node::~node(void)
{
   for(simple_tuple_map::iterator it(tuples.begin()), end(tuples.end()); it != end; it++)
      delete it->second;
   for(aggregate_map::iterator it(aggs.begin()), end(aggs.end()); it != end; it++)
      delete it->second;
}

void
node::dump(ostream& cout) const
{
   cout << get_id() << endl;

   for(simple_tuple_map::const_iterator it(tuples.begin());
      it != tuples.end();
      ++it)
   {
      it->second->dump(cout);
   }
}

typedef pair<string, tuple_trie*> str_trie;

static inline bool
trie_comparer(const str_trie& p1, const str_trie& p2)
{
	return p1.first < p2.first;
}

void
node::print(ostream& cout) const
{
	typedef list<str_trie> list_str_trie;
	list_str_trie ordered_tries;

	// order tries by predicate name
   for(simple_tuple_map::const_iterator it(tuples.begin());
      it != tuples.end();
      ++it)
   {
		tuple_trie *tr(it->second);
		predicate_id id(it->first);
      const predicate *pred(All->PROGRAM->get_predicate(id));

		ordered_tries.push_back(str_trie(pred->get_name(), tr));
	}

	ordered_tries.sort(trie_comparer);

	if( !debugger::isInDebuggingMode()&&!debugger::isInSimDebuggingMode()&&
        !debugger::isInMpiDebuggingMode()){
	  cout << "--> node " << get_translated_id() << "/(id " << get_id()
        << ") (" << this << ") <--" << endl;
	} else {
	  cout << "CONTENTS AT NODE " << get_translated_id() << ":" << endl;
	}

	for(list_str_trie::const_iterator it(ordered_tries.begin());
			it != ordered_tries.end();
			++it)
	{
		tuple_trie *tr(it->second);

		if(!tr->empty())
			tr->print(cout);
	}
}

bool
node::empty(void) const {
    for(simple_tuple_map::const_iterator it(tuples.begin());
        it != tuples.end();
        ++it) {
        if(!(it->second)->empty())
            return false;
    }

    return true;
}

void
node::init(void)
{
}

ostream&
operator<<(ostream& cout, const node& node)
{
   node.print(cout);
   return cout;
}

}


// Local Variables:
// mode: C++
// indent-tabs-mode: nil
// End:
