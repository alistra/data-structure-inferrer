struct ds;
struct dselem;
typedef struct ds *ds;
typedef struct dselem *dselem;

void insert_d(ds, dstype);
void update_d(ds, dstype);
void delete_d(ds, dstype);
dselem search_d(ds, dstype);
void delmax_d(ds);
dselem max_d(ds);
void delmin_d(ds);
dselem min_d(ds);

void delete_de(dselem);
void update_de(dselem);
