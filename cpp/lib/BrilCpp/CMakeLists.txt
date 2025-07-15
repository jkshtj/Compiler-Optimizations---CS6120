add_library(brilcpp
    Core.cpp
)

target_include_directories(brilcpp PUBLIC 
    ${PROJECT_SOURCE_DIR}/include/BrilCpp
)

target_link_libraries(brilcpp PUBLIC nlohmann_json::nlohmann_json)
