typedef unsigned long grib_handle;
typedef unsigned long size_t;

// multio_grib2 dictionary
int multio_grib2_dict_create( void** dict, char* dict_type );
int multio_grib2_dict_set( void* dict, const char* key, const char* value );
int multio_grib2_dict_get( void* dict, const char* key, char** value );
int multio_grib2_dict_has( void* dict, const char* key, int* has );
int multio_grib2_dict_iterate( void* dict, void** iterator, char** key, char** value );
int multio_grib2_dict_destroy_iterator( void* dict, void** iterator );
int multio_grib2_dict_destroy( void** dict );

int multio_grib2_dict_set_geometry( void* dict, const void* geom_dict );

// Initialize all the options to a relevant default
int multio_grib2_init_options( void** opt_dict );

// options dict can be null, in this case default options can be applied
int multio_grib2_encoder_open( void* opt_dict, void** multio_grib2 );
int multio_grib2_encoder_extract_metadata( void* multio_grib2, grib_handle* grib, void** mars_dict, void** par_dict );
int multio_grib2_encoder_encode64( void* multio_grib2, void* mars_dict, void* par_dict, double* data, size_t data_len, grib_handle* out_handle );
int multio_grib2_encoder_encode32( void* multio_grib2, void* mars_dict, void* par_dict, float*  data, size_t data_len, grib_handle* out_handle );
int multio_grib2_encoder_close( void** multio_grib2 );
