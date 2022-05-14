import PackagePlugin

@main
struct CitronParserGenerator: BuildToolPlugin {

  func createBuildCommands(context: PluginContext, target: Target) throws -> [Command] {
    guard let target = target as? SourceModuleTarget else { return [] }
    let inputFiles = target.sourceFiles.filter({ [ "citron", "y"].contains($0.path.extension) })
    return try inputFiles.map {
      let inputFile = $0
      let inputPath = inputFile.path
      let outputName = inputPath.stem + ".swift"
      let outputPath = context.pluginWorkDirectory.appending(outputName)
      return .buildCommand(
        displayName: "Generating \(outputName) from \(inputPath.lastComponent)",
        executable: try context.tool(named: "citron").path,
        arguments: [ "\(inputPath)", "-o", "\(outputPath)" ],
        inputFiles: [ inputPath, ],
        outputFiles: [ outputPath ]
      )
    }
  }}
