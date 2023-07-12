#' @title `r lifecycle::badge("maturing")` Create an AWS EC2 launcher object.
#' @export
#' @family launchers
#' @description Create an `R6` AWS EC2 launcher object.
aws_launcher_ec2 <- function(
  name = NULL,
  verbose = FALSE,
  access_key_id = "your-aws-access-key",
  secret_access_key = "your-aws-secret-key",
  ami_id = "your-aws-ec2-ami-ID",
  instance_type = "t2.micro",
  key_name = "your-ec2-key-pair",
  keyfile = "your-private-key-file.pem"
) {
  name <- as.character(name %|||% paste("aws", sample(LETTERS, 5, replace = TRUE), collapse = ""))
  launcher <- aws_class_launcher_ec2$new(
    name = name,
    verbose = verbose,
    access_key_id = access_key_id,
    secret_access_key = secret_access_key,
    ami_id = ami_id,
    instance_type = instance_type,
    key_name = key_name,
    keyfile = keyfile
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("maturing")` AWS EC2 launcher class
#' @export
#' @family launchers
#' @description `R6` class for the AWS EC2 launcher.
aws_class_launcher_ec2 <- R6::R6Class(
  classname = "aws_class_launcher_ec2",
  inherit = crew::crew_class_launcher,
  cloneable = FALSE,
  public = list(
    verbose = NULL,
    access_key_id = NULL,
    secret_access_key = NULL,
    ami_id = NULL,
    instance_type = NULL,
    key_name = NULL,
    keyfile = NULL,

    ec2 = NULL,

    initialize = function(name = NULL, verbose = NULL, access_key_id = NULL, secret_access_key = NULL, ami_id = NULL, instance_type = NULL, key_name = NULL, keyfile = NULL) {
      super$initialize(name = name)
      self$verbose <- verbose
      self$access_key_id <- access_key_id
      self$secret_access_key <- secret_access_key
      self$ami_id <- ami_id
      self$instance_type <- instance_type
      self$key_name <- key_name
      self$keyfile <- keyfile

      self$ec2 <- paws::ec2(
        config = list(
          credentials = list(
            creds = list(
              access_key_id = self$access_key_id,
              secret_access_key = self$secret_access_key
            )
          )
        )
      )
    },

    validate = function() {
    super$validate()
    # Add validation for the EC2 instance configuration.
    crew::crew_assert(
        self$ec2,
        !is.null(.),
        message = "EC2 instance is not properly configured."
    )
    },

    launch_worker = function(call, name, launcher, worker, instance) {
    # Fetch the running instances
    instances <- self$ec2$describe_instances(
        Filters = list(
        list(
            Name = "instance-state-name",
            Values = c("running")
        )
        )
    )
    
    if (length(instances$Reservations) > 0) {
        # connect to an already running instance, I prefer this method as it will not need to spin up and download R
        instance <- instances$Reservations[[1]]$Instances[[1]]
    } else {

        # Script to install R TODO[This will change based on AMI choice.. maybe just ask the user to provide?]
        user_data <- '#!/bin/bash
                sudo yum update -y
                sudo yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
                sudo yum install -y R
                sudo amazon-linux-extras install epel -y'
        
        # start a new instance
        instance <- self$ec2$run_instances(
        ImageId = "your-aws-ec2-ami-ID", # replace with your preferred AMI TODO[Ask Will about preferred AMI]
        MinCount = 1,
        MaxCount = 1,
        InstanceType = "t2.micro", # replace with your preferred instance type TODO[Free tier for now, should this be dynamic]
        KeyName = "your-ec2-key-pair" # I have a key pair, TODO[Ask Will about ]
        UserData = base64enc::base64encode(charToRaw(user_data))
        )
        
        Sys.sleep(60) # give it time to start [May take more than 60 seconds for it to spin up and download R, yikes]
    }

    session <- ssh::ssh_connect(
        host = instance$PublicDnsName,
        keyfile = "your-private-key-file.pem" #this may be a challenge for a new instance and user.. TODO[Ask Will about pem file]
    )

    ssh::ssh_exec_wait(session, command = paste("Rscript -e '", call, "'"))

    return(list(instance = instance, session = session))
    },

    terminate_worker = function(handle) {
    # Disconnect from the instance's SSH session
    if (!is.null(handle$session)) {
        ssh::ssh_disconnect(handle$session)
    }

    # Terminate the instance
    if (!is.null(handle$instance)) {
        self$ec2$terminate_instances(
        InstanceIds = handle$instance$InstanceId
        )
    }
    }

