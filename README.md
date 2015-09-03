# Elip
An Erlang Application to lookup network name with given network configuration.

# Usages
If V3 network is 10.5.0.0/16, To lookup V3 network codes is following codes.

```erlang
1> elip:lookup("10.5.1.1").
{ok,{found,"V3"}}
2> elip:lookup("10.50.1.1").
{ok,not_found}
```

# Configuration file format
```erlang
{
    {name, "V0"},
    {description, "Office 1"},
    {network, ["10.1.0.0/16", "10.2.0.0/16"]}
}.

{
    {name, "V1"},
    {description, "Office 2"},
    {network, ["10.3.0.0/16"]}
}.

{
    {name, "V2"},
    {description, "Office 3"},
    {network, ["10.4.0.0/16", "10.41.0.0/16"]}
}.

{
    {name, "V3"},
    {description, "Office 4"},
    {network, ["10.5.0.0/16", "10.51.0.0/16"]}
}.
```

# To run
```bash
$ make run
```
