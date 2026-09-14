################################################################################
# If you want to use GPU to speed up, you will need to use Torch: https://torch.mlverse.org/docs/
# Here is a link for the setup: https://torch.mlverse.org/docs/articles/installation


# There are two options for Windows User (Mac users can skip to the next section):
# Easier way is to download a prebuilt CUDA12.8:
options(timeout = 600) # increasing timeout is recommended since we will be downloading a 2GB file.
# For Windows and Linux: "cpu", "cu128" are the only currently supported
# For MacOS the supported are: "cpu-intel" or "cpu-m1"
kind <- "cu128"
version <- available.packages()["torch","Version"]
options(repos = c(
  torch = sprintf("https://torch-cdn.mlverse.org/packages/%s/%s/", kind, version),
  CRAN = "https://cloud.r-project.org" # or any other from which you want to install the other R dependencies.
))
install.packages("torch")


# Alternative (hard way, not recommended)
# 1. Open Command Prompt (In windows: Win+R -> cmd) and run:
#  nvidia-smi
# You should see CUDA on the top right corner (most modern NVIDIA GPUs do)
# My machine support CUDA version up to 12.9
# 2. Install CUDA toolkit: For now, 11/9/2025, have to be version 12.8.1
# https://developer.nvidia.com/cuda-12-8-1-download-archive?target_os=Windows&target_arch=x86_64&target_version=11&target_type=exe_local
# You can find the downloaded files here: C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA
# 3. Install cuDNN: For now, 11/9/2025, the latest version is 9.15.0
# https://developer.nvidia.com/cudnn-downloads?target_os=Windows&target_arch=x86_64&target_version=11&target_type=exe_local# https://developer.nvidia.com/nvidia-cuda-toolkit-developer-tools-mac-hosts
# You can find the downloaded files here: C:\Program Files\NVIDIA\CUDNN


################################################################################
# After Windows setup or you have a Mac, directly install torch
install.packages("torch")

library(torch)
# You will be promt to download additional software. Click "Yes".
install_torch(cuda = TRUE)


################################################################################
# IMPORTANT POINT: Memory Management
# https://torch.mlverse.org/docs/articles/memory-management
# R is lazy on collecting garbage and tensor might consume your computer memory
# Remember that we were manually calling "gc()" to do garbage collection
# Here are several ways dealing with it for tensor:
# 1. CPU (This option must be set before calling "library(torch)")
# 4000 here means the max for tensor is 4GB before it is cleared, you can make it smaller for larger data
options(torch.threshold_call_gc = 4000)

# 2. GPU
# Automatic and can leave to default.
# Windows version (CUDA) can have some customized settings and debugging tools.


################################################################################
# Check whether GPU setup is successful: Note: Mac and Windows are slightly different
# Check for GPU availability for Windows and Mac
if (cuda_is_available()) {
  cat("GPU works for Windows.\n")
  device <- "cuda"
} else if (backends_mps_is_available()) {
  cat("GPU works for Mac.\n")
  device <- "mps"
} else {
  cat("GPU does not work.\n")
  device <- "cpu"
}

# Create a torch tensor (number)
torch_tensor(1, device = device)

