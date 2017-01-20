--
--  Copyright (C) 2016 Christiane Kuhn, secunet AG
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Types.Metas.Meta_Subject_Vectors;
with Types.Metas.Meta_Subject_Package;

package Auxiliary.Subjects
is

   --  Return subject with given name, raise exception if no such subject
   --  exists.
   function Subject_By_Name
     (Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Name     : String)
      return Types.Metas.Meta_Subject_Package.Meta_Subject_Type;

   No_Such_Subject : exception;

end Auxiliary.Subjects;
