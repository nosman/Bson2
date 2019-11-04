module Primitives = Primitives

module type BsonReader = S.BsonReader

module type BsonWriter = S.BsonWriter

module Binary = struct
    module Reader = Binary_reader.Reader

    module Writer = Binary_writer.Writer
end

module ExtendedJson = struct
    module Reader = Json_reader
end
