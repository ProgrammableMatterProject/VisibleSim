#include "api.hpp"

namespace api
{

	void 
	init(sched::base *schedular)
	{
	}

	void 
	send_message(db::node* from, const db::node::node_id to, db::simple_tuple* stpl)
	{
		printf("SM?\n");
	}

	bool 
	poll() 
	{
		return false;
	}

	void 
	set_color(db::node *n, const int r, const int g, const int b)
	{
		printf("set color?\n");
	}

	void 
	check_pre(sched::base *scheduler)
	{
		printf("check_pre\n");
	}

	bool 
	isReady()
	{
		return true;
	}

	bool ensembleFinished() 
	{
		return true;
	}

}
