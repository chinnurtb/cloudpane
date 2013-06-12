{application,cloudpane,
             [{description,"Cloudpane - For a variety of business development"},
              {vsn,"0.0.1"},
              {modules,[cloudpane,cloudpane_app,cloudpane_sup,cp_crypto,
                        cp_time,cp_uuid,ej,misc]},
              {registered,[]},
              {applications,[kernel,stdlib,inets,crypto,mochiweb,webmachine]},
              {mod,{cloudpane_app,[]}},
              {env,[]}]}.
