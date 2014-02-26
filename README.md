# CGroup Introduction

With this library you can interact directly with Linux's [cgroups](https://www.kernel.org/doc/Documentation/cgroups/cgroups.txt) feature, access basic information about them including the associated PIDs, and add PIDs to a group. What are cgroups?

> Control Groups provide a mechanism for aggregating/partitioning sets of
> tasks, and all their future children, into hierarchical groups with
> specialized behaviour.

The library uses types to distinguish between data originating from anonymous sources, and data from other parts of the API. Doing this keeps everything safe, but also limits the performance impact of using the API on untainted data. 

## Methods


* fetching `allCGroups`,
* `listTasks` will list PIDs associated with a particular cgroup
* and `addTask` will add a particular PID to a given CGroup.
* More to come! This library will eventually expose the entire cgroup VFS, including resource limits, prioritization, accounting, freezing, checkpointing, and restarting.