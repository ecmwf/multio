import os

from .lib import ffi, lib
from .config import Config
from .handler import Handler
from .metadata import Metadata

class Multio:
    """This is the main interface class for Multio that users will interact with"""

    def __init__(self, config):
        
        self.__conf = Config(**config)

        self.__handle = Handler(self.__conf)

        self.__metadata = None

    def create_metadata(self, md=None):

        self.__metadata = Metadata(self.__handle, md=md)

    