#include "dictionary.h"
/* Dictionary implementation */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define INITIAL_HASH_SIZE 8
#define MAX_LOAD_FACTOR 2
#define SCALE_FACTOR 2

static DR get_item(const char *key);
static int insert_or_update(DR new_item);
static void mark_cycle(DR item);
static void unmark_cycle(DR item);
static int hash(const char *key);
static void insert_at_front(DR *list, DR new_item);
static DR remove_from_front(DR *list);
static void resize(int size);

int h_size;
int num_items;
DR *hash_tab;

char * str_dupl(const char *s) /* make a duplicate of s */
{
	char *p;
	p = (char *)malloc(strlen(s) + 1); /* +1 for ’\0’ */
	if (p != NULL)
		strcpy(p, s);
	return p;
}

char * find_value_for_item(DR item) 
{
	if (item == NULL) {
		return NULL;
	}

	char *value = NULL;
	DR t_item = item;
	
	//otherwise follow the trail
	while (t_item != NULL) {

		//if value is int_const, just take it
		if (t_item->tag == INT_CONST) {
			char int_value[512];
			//value = itoa(t_item->u.intconstval, int_value, 10);
#ifdef _WIN32
			_snprintf(value, sizeof(int_value), "%ld", t_item->u.intconstval);
#else
			snprintf(value, sizeof(int_value), "%ld", t_item->u.intconstval);
#endif
			break;
		}
		//if value is str_const, just take it
		else if (t_item->tag == STR_CONST) {
			value = str_dupl(t_item->u.strconstval);
			break;
		}
		//if value is identifier, try find next DR, take identifier if none found or a cycle
		else if (t_item->tag == ID) {

			//if item is in cycle, just return it's key
			if (t_item->in_cycle == TRUE) {
				value = str_dupl(t_item->key);
				break;
			} //else

			//find next item
			DR t_item2 = get_item(t_item->u.idval);
			//doesn't exist? take current item's idval
			if (t_item2 == NULL) {
				value = str_dupl(t_item->u.idval);
				break;
			}
			else {
				t_item = t_item2;
			}
		}
	}

	return value;
}

char * find_value_for_key(const char *key) 
{
	return find_value_for_item(get_item(key));
}

void init_dict()
{
	h_size = 0;
	hash_tab = NULL;
	resize(INITIAL_HASH_SIZE);
	num_items = 0;
}

// Add a key with an integer constant value to the dictionary
void add_int_to_dict(const char *key, long val)
{
	DR p = (DR)malloc(sizeof(DICT_REC));
	p->in_cycle = FALSE;
	p->key = key;
	p->tag = INT_CONST;
	p->u.intconstval = val;
	if (insert_or_update(p) == 0)
		fprintf(stderr, "Warning: redefinition of %s to %ld\n", key, val);
}

// Add a key with a string constant value to the dictionary
void add_str_to_dict(const char *key, const char *val)
{
	DR p = (DR)malloc(sizeof(DICT_REC));
	p->in_cycle = FALSE;
	p->key = key;
	p->tag = STR_CONST;
	p->u.strconstval = val;
	if (insert_or_update(p) == 0)
		fprintf(stderr, "Warning: redefinition of %s to %s\n", key, val);
}

// Add a key with an identifier value to the dictionary
void add_id_to_dict(const char *key, const char *val)
{
	DR p = (DR)malloc(sizeof(DICT_REC));
	p->in_cycle = FALSE;
	p->key = key;
	p->tag = ID;
	p->u.idval = val;
	if (insert_or_update(p) == 0)
		fprintf(stderr, "Warning: redefinition of %s to %s\n", key, val);
}

// Output the substituted value of a defined ID to the output stream
// returns TRUE if substituted, otherwise FALSE
boolean output_substitution(FILE *stream, const char *id)
{
	debug1("found id, %s; checking for a matching define value\n", id);
	char *value = find_value_for_key(id);
	if (value != NULL) {
		debug2("replacing id, %s with value, %s\n", id, value);
		fputs(value, stream);
		fflush(stream);
		return TRUE;
	} //else
	debug1("no match found for id, %s\n", id);
	return FALSE;
}


/* Local routines */

/* Returns NULL if item not found */
static DR get_item(const char *key)
{
	int index = hash(key);
	DR p = hash_tab[index];
	debug1("get_item: p %s NULL\n", p == NULL ? "==" : "!=");
	while (p != NULL && strcmp(key, p->key))
		p = p->next;
	return p;
}

/* Returns the number of new items inserted: 1 if new insertion; 0 otherwise */
static int insert_or_update(DR new_item)
{
	int ret;
	const char *key = new_item->key;
	int index = hash(key);
	DR p = hash_tab[index], *prev = hash_tab + index;

	debug1("insert_or_update(key=%s) called\n", new_item->key);
	while (p != NULL && strcmp(key, p->key)) {
		prev = &p->next;
		p = p->next;
	}

	if (p == NULL) {
		/* Insertion */
		debug1("  p is NULL -- inserting at index %d\n", index);
		insert_at_front(prev, new_item);
		debug("    done\n");
		if (num_items++ > MAX_LOAD_FACTOR*h_size) {
			debug("    resizing\n");
			resize(SCALE_FACTOR*h_size);
		}
		ret = 1;
	}
	else {
		/* Update */
		debug1("  p is not NULL -- updating at index %d\n", index);
		unmark_cycle(p);
		free(remove_from_front(prev));
		insert_at_front(prev, new_item);
		ret = 0;
	}

	mark_cycle(new_item);
	return ret;
}

// Marks the new cycle, if there is one
static void mark_cycle(DR item)
{
	item->in_cycle = TRUE;

	DR t_item = item;
	boolean found_cycle = TRUE;

	while (t_item != NULL) {
		t_item->in_cycle = TRUE;

		//if value is an id
		if (t_item->tag == ID) {
			t_item = get_item(t_item->u.idval); //try get item matching it
		}
		else { //otherwise unset t_item
			t_item = NULL;
		}

		//if can't find next item, no cycle, break
		if (t_item == NULL) {
			found_cycle = FALSE;
			break;
		}

		//if we reached another node in cycle, we are in a cycle, break
		if (t_item->in_cycle == TRUE) {
			break;
		}
	}

	if (found_cycle == FALSE) {
		unmark_cycle(item);
	} else {
		debug("    cycle marked\n");
	}
}

// Unmark an existing cycle
static void unmark_cycle(DR item)
{
	DR t_item = item;
	while (t_item != NULL && t_item->in_cycle == TRUE) {
		t_item->in_cycle = FALSE;
		if (t_item->tag == ID) {
			t_item = get_item(t_item->u.idval);
		}
	}
}


static int hash(const char *key)
{
	int sum = 0;
	for (; *key; key++)
		sum = (37 * sum + *key) % h_size;
	return sum;
}

static void insert_at_front(DR *list, DR new_item)
{
	new_item->next = *list;
	*list = new_item;
}

static DR remove_from_front(DR *list)
{
	DR ret = *list;
	*list = (*list)->next;
	return ret;
}

static void resize(int size)
{
	int i;

	DR *temp = hash_tab;
	int temp_size = h_size;

	h_size = size;
	hash_tab = (DR *)malloc(h_size * sizeof(DR));
	for (i = 0; i < h_size; i++)
		hash_tab[i] = NULL;

	// This only occurs on the initial sizing, with empty dictionary
	if (temp == NULL)
		return;

	for (i = 0; i < temp_size; i++)
	while (temp[i] != NULL) {
		int index = hash(temp[i]->key);
		insert_at_front(hash_tab + index, remove_from_front(temp + i));
	}
	free(temp);
}

void print_record(DR dr)
{
	if (dr == NULL) {
		printf("[NODE]NULL");
		return;
	} //else

	printf("[NODE]key=%s, ", dr->key);
	if (dr->tag == INT_CONST) {
		printf("value=%ld, type=INT_CONST, ", dr->u.intconstval);
	}
	else if (dr->tag == STR_CONST) {
		printf("value=%s, type=STR_CONST, ", dr->u.strconstval);
	}
	else if (dr->tag == ID) {
		printf("value=%s, type=ID, ", dr->u.idval);
	}
	printf("in_cycle=%d", dr->in_cycle);
	if (dr->next != NULL) {
		printf("->");
	}
	else {
		printf("\n");
	}
}

void print_contents()
{
	DR dr;
	int i;
	for (i = 0; i < h_size; i++) {
		dr = hash_tab[i];
		while (dr != NULL) {
			print_record(dr);
			dr = dr->next;
		}
	}
}

void process_define_identifier(char *key, char *value)
{
	add_id_to_dict(key, value);
}

void process_define_int_const(char *key, long value)
{
	add_int_to_dict(key, value);
}

void process_define_str_const(char *key, char *value)
{
	add_str_to_dict(key, value);
}
