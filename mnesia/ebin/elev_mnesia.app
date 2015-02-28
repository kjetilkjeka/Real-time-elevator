{application, elev_mnesia,
 [{description, "Mnesia for elevator system"},
  {vsn, "1.0.1"},
  {modules, [elev_mnesia, elev_mnesia_sup]},
  {applications, [stdlib, kernel, mnesia]}]}.
