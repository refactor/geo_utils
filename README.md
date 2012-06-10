geo_utils
=========

Geospatial Utilities


application:start(compiler), application:start(syntax_tools), application:start(lager).
lager:set_loglevel(lager_console_backend, debug).
{ok,Pid} = img_scanner:start_link(global_mercator, "../tz/tzh.tif").

