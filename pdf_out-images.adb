with GID;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body PDF_Out.Images is

  procedure Image_ref(pdf: in out PDF_Out_Stream; file_name: String; image_index: out Positive) is

    procedure Insert(file_name: String; node: in out p_Dir_node) is
    begin
      if node = null then
        pdf.img_count:= pdf.img_count + 1;
        node:= new Dir_node'
          ( (name_len          => file_name'Length,
             left              => null,
             right             => null,
             file_name         => file_name,
             image_index       => pdf.img_count,
             pdf_object_index  => 0,  --  0 = not yet insterted into the PDF stream
             local_resource    => True)
          );
          image_index:= pdf.img_count;
      elsif file_name > node.file_name then
        Insert( file_name, node.right );
      elsif file_name < node.file_name then
        Insert( file_name, node.left );
      else
        --  Name found, image was already referenced (above in the document)
        image_index:= node.image_index;
        node.local_resource:= True;
      end if;
    end Insert;
  begin
    Insert(file_name, pdf.img_dir_tree);
  end Image_ref;

  function Get_pixel_dimensions(image_file_name: String) return Rectangle is
    use Ada.Streams.Stream_IO;
    file: File_Type;
    i: GID.Image_descriptor;
  begin
    Open(file, In_File, image_file_name);
    GID.Load_image_header(i, Stream(file).all, try_tga => False);
    Close(file);
    return (0.0, 0.0, Real(GID.Pixel_width(i)), Real(GID.Pixel_height(i)));
  end Get_pixel_dimensions;

  procedure Traverse_private( pdf: PDF_Out_Stream ) is

    procedure Traverse( p: p_Dir_node ) is
    begin
      if p /= null then
        Traverse(p.left);
        Action_private(p.all);
        Traverse(p.right);
      end if;
    end Traverse;

  begin
    Traverse(pdf.img_dir_tree);
  end Traverse_private;

  procedure Clear_image_directory( pdf: in out PDF_Out_Stream ) is
    procedure Clear( p: in out p_Dir_node ) is
      procedure Dispose is new Ada.Unchecked_Deallocation(Dir_node, p_Dir_node);
    begin
      if p /= null then
        Clear(p.left);
        Clear(p.right);
        Dispose(p);
      end if;
    end Clear;
  begin
    Clear(pdf.img_dir_tree);
  end Clear_image_directory;

  procedure Clear_local_resource_flag( dn: in out Dir_node ) is
  begin
    dn.local_resource:= False;
  end;

  procedure Clear_local_resource_flags( pdf: PDF_Out_Stream ) is
    procedure Traverse_and_clear is new Traverse_private(Clear_local_resource_flag);
  begin
    Traverse_and_clear(pdf);
  end;

  procedure Insert_unloaded_local_images( pdf: in out PDF_Out_Stream ) is

    procedure Insert_Image_as_XObject(file_name: String) is
      file_size: Natural;
      use Ada.Streams.Stream_IO;
      file: File_Type;
      i: GID.Image_descriptor;
      use GID;
    begin
      Open(file, In_File, file_name);
      file_size:= Integer(Size(file));
      GID.Load_image_header(i, Stream(file).all, try_tga => False);
      Close(file);
      if GID.Format(i) /= GID.JPEG then
        Raise_Exception(
          Not_implemented'Identity,
          "So far only JPEG images can be inserted. This image is of type " &
          GID.Detailed_format(i) & ", file name = " & file_name
        );
      end if;
      New_object(pdf);
      WL(pdf,
        "<< /Type /XObject /Subtype /Image /Width " &
        Img(GID.Pixel_width(i)) & " /Height " & Img(GID.Pixel_height(i)) &
        " /ColorSpace /DeviceRGB /BitsPerComponent " & Img(GID.Bits_per_pixel(i) / 3) &
        " /Length " & Img(file_size) & " /Filter /DCTDecode >>"
      );
      WL(pdf, "stream");
      Copy_file(file_name, pdf.pdf_stream.all);
      WL(pdf, "");
      WL(pdf, "endstream");
      WL(pdf, "endobj");
    end Insert_Image_as_XObject;

    procedure Insert_unloaded_local_image( dn: in out Dir_node ) is
    begin
      if dn.local_resource and then dn.pdf_object_index = 0 then
        Insert_Image_as_XObject(dn.file_name);
        dn.pdf_object_index:= pdf.objects;
      end if;
    end;

    procedure Traverse_and_load is new Traverse_private(Insert_unloaded_local_image);

  begin
    Traverse_and_load(pdf);
  end;

end PDF_Out.Images;
