import os

from .config import Config
from .handler import Handler
from .lib import ffi, lib
from .metadata import Metadata


class Multio:
    """This is the main interface class for Multio that users will interact with"""

    def __init__(self, **kwargs):
        self.__conf = Config(**kwargs)

        self.__handle = Handler(self.__conf)

        self.__dummy_metadata_field = Metadata(self.__handle, md={})
        self.__dummy_metadata_domain = Metadata(self.__handle, md={})
        self.__dummy_metadata_mask = Metadata(self.__handle, md={})
        self.__dummy_metadata_flush = Metadata(self.__handle, md={})
        self.__dummy_metadata_notification = Metadata(self.__handle, md={})

    def __version__(self):
        tmp_str = ffi.new("char**")
        lib.multio_version(tmp_str)
        versionstr = ffi.string(tmp_str[0]).decode("utf-8")
        return versionstr

    def set_conf_path(self, conf_path):
        self.__conf.set_conf_path(conf_path)

    def start_server(self):
        self.__conf.start_server()

    # def create_metadata(self, md=None):
    #     self.__metadata = Metadata(self.__handle, md=md)

    # def delete_metadata(self):
    #     self.__metadata.delete_metadata()

    def open_connections(self):
        self.__handle.open_connections()

    def close_connections(self):
        self.__handle.close_connections()

    def flush(self, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.flush(self.__dummy_metadata_flush)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.flush(md)
            md.delete_metadata()

    def notify(self, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.notify(self.__dummy_metadata_notification)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.notify(md)
            md.delete_metadata()

    def write_domain(self, data, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.write_domain(self.__dummy_metadata_domain, data)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.write_domain(md, data)
            md.delete_metadata()

    def write_mask(self, data, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.write_mask(self.__dummy_metadata_mask, data)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.write_mask(md, data)
            md.delete_metadata()

    def write_mask_float(self, data, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.write_mask_float(self.__dummy_metadata_mask, data)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.write_mask_float(md, data)
            md.delete_metadata()

    def write_mask_double(self, data, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.write_mask_double(self.__dummy_metadata_mask, data)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.write_mask_double(md, data)
            md.delete_metadata()

    def write_field(self, data, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.write_field(self.__dummy_metadata_field, data)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.write_field(md, data)
            md.delete_metadata()

    def write_field_float(self, data, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.write_field_float(self.__dummy_metadata_field, data)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.write_field_float(md, data)
            md.delete_metadata()

    def write_field_double(self, data, metadata=None):
        if metadata is None or len(metadata) == 0:
            self.__handle.write_field_double(self.__dummy_metadata_field, data)
        else:
            md = Metadata(self.__handle, md=metadata)
            self.__handle.write_field_double(md, data)
            md.delete_metadata()

    def write_grib(self, data):
        self.__handle.write_grib_encoded(data)

    def field_accepted(self, metadata=None):
        if metadata is None or len(metadata) == 0:
            return self.__handle.field_accepted(self.__dummy_metadata)
        else:
            md = Metadata(self.__handle, md=metadata)
            ret = self.__handle.field_accepted(md)
            md.delete_metadata()
            return ret
