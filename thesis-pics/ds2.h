// use #defines the dstype in its code before including this

struct dselem;
struct ds;

typedef struct ds *ds;
typedef struct dselem *dselem;

ds init_d();
dselem insert_d(ds, dstype);
dselem update_d(ds, dselem, dstype);
void delete_elem_d(ds, dselem);
void delete_val_d(ds, dstype);
void delmax_d(ds);
void delmin_d(ds);
dselem search_d(ds, dstype);
dselem max_d(ds);
dselem min_d(ds);
dselem successor_d(ds, dselem);
dselem predecessor_d(ds, dselem);
