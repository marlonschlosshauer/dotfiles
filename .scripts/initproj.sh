#Setup folder structure
#Create both SLN and CSPROJ files
#Initialise git repo 

#Set up variable 
if [ $# == 0 ]
then
	name="Project"
else
	name=$1
fi

#Set up folder structure
echo "Creating variable"
mkdir $name
cd $name

#Set up sln and csproj files
echo "Setting up Solution file"
dotnet new sln
dotnet new console -n $name --target-framework-override netcoreapp2.0
dotnet new nunit -n Testing --target-framework-override netcoreapp2.0 

#Initialise Git repo
echo "Creating Git Repo"
git init 

#Move and rename Program.cs
echo "Renaming Program.cs"
mv $name Working
cd Working 
mkdir src
mv Program.cs $name.cs
mv $name.cs src

#Rename Testing.csproj
echo "Renaming .CSPROJ file"
cd ..
cd Testing
mv Testing.csproj $name.Test.csproj

#Create template Test file
echo "Create template Test file for NUnit"
mkdir src
cd src
touch $1Test.cs
echo "
using System;
using NUnit.Framework;

/// <summary>
/// Test $1
/// </summary>
///
[TestFixture]
public class $1Test
{

	private $1 Test;

	/// <summary>
	/// Initializes $1 test object
	/// </summary>
	///
	[SetUp]
	protected void SetUp()
	{
		Test = new $1();
	}

	/// <summary>
	/// Test ToString() function
	/// </summary>
	///
	[Test]
	public void TestToString()
	{
		throw new NotImplementedException();
	}
}" >> $1Test.cs
cd ..
cd ..

#Add project references
echo "Creating project references"
dotnet sln add Working/$name.csproj
cd Testing
dotnet add reference ../Working/$name.csproj
cd ..
dotnet sln add Testing/$name.Test.csproj
cd ..

echo "Done."
