{application, ewalker,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             ewalker_app,
             ewalker_sup,
             ewalker_watcher,
             ewalker_writer
             ,ewalker
             ,atom
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  sasl
                 ]},
  {mod, { ewalker_app, []}},
  {env, [
    {output, "filelist.atom"},
    {folders, []},
    {name, "Files"},
    {mapping, [
                 {"file:///", "/"},
                 {"http://moon.localdomain/dav/", "/public/"}
              ]}
    
  ]}
 ]}.
