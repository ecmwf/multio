
module multio

    use multio_nemo, only: multio_open_connections, multio_close_connections, multio_write_step_complete, &
         multio_init_server, multio_metadata_set_int_value, multio_init_client, multio_set_domain, &
         multio_write_field, multio_field_is_active

end module multio
