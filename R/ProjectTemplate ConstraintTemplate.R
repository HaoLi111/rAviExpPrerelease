#Constraints template

constraint.template = list(W_S = NA,
                           turn = turn.template,
                           energy_level = energy_level.template,
                           service_ceiling = service_ceiling.template,
                           cruise_v = cruise_v.template,
                           climb = climb.template)



loadCase.template = list(Structual = NA,
                         Heat = NA)

projectInfo.template = list(ID = NA,
                            Name = NA,
                            Description = NA,
                            Author = NA,
                            Date = NA,
                            license = NA)

requirement.template = list(constraint = constraint.template,
                            loadCase = loadCase.template,
                            projectInfo = projectInfo.template)


fuse_requirement <-function(constraint,loadCase,projectInfo) list(constraint = constraint, loadCase = loadCase,projectInfo = projectInfo)


