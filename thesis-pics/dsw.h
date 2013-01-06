struct ds;
typedef struct ds *ds;
typedef weight int;

const DSINF_IMPORTANT = 100;
const DSINF_IGNORE = 0;

ds init_d();
void insert_dw(ds, dstype, weight);
void update_dw(ds, dstype, dstype, weight);
void delete_dw(ds, dstype, weight);
void delmax_dw(ds, weight);
void delmin_dw(ds, weight);
dstype search_dw(ds, dstype, weight);
dstype max_dw(ds, weight);
dstype min_dw(ds, weight);
