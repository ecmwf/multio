import pymultio

conf = pymultio.Config(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])

handle = pymultio.Handler(conf)

md = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'Error' : (0,1)}

md = pymultio.Metadata(handle, md)
md.metadata_set_string('category', 'path')
md.metadata_set_int('globalSize', 4)
md.metadata_set_int('level', 1)
md.metadata_set_int('step', 1)
md.metadata_set_string('trigger', 'step')
md.metadata_set_double('missingValue', 0.0)
md.metadata_set_bool('bitmapPresent', False)
md.metadata_set_int('bitsPerValue', 16)
md.metadata_set_bool('toAllServers', False)
md.metadata_set_string('name', 'test')


handle.write_field_double(md, [1.0,2.0,3.0,4.0], 4)
handle.notify(md)
handle.flush(md)
handle.field_accepted(md, False)
